;;;; recipe-repository.lisp --- Unit tests for the recipe repository.
;;;;
;;;; Copyright (C) 2019, 2020 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:build-generator.model.project.test)

;;; Mocks

(defclass mock-repository (recipe-repository)
  ((%pathnames :initarg  :pathnames
               :accessor pathnames)))

(defmethod probe-recipe-pathname ((repository mock-repository) (pathname pathname))
  (when (find pathname (pathnames repository))
    (if (wild-pathname-p pathname)
        (list `(:directory ,pathname))
        (list `(:probe ,pathname)))))

(defun make-populated-mock-repository (root-directory mode-or-modes files &rest parents)
  (populate-recipe-repository!
   (make-instance 'mock-repository
                  :root-directory (pathname root-directory)
                  :mode           (ensure-mode mode-or-modes)
                  :pathnames      (map 'list (rcurry #'merge-pathnames root-directory)
                                       files)
                  :parents        parents)))

(defun make-mock-repository-stack (mode-or-modes spec)
  (let+ (((&labels+ rec ((name files &rest parents))
            (apply #'make-populated-mock-repository
                   name mode-or-modes files (map 'list #'rec parents)))))
    (rec spec)))

;;; Tests

(def-suite* :build-generator.model.project.concrete-syntax.recipe-repository
  :in :build-generator.model.project.concrete-syntax)

(test make-populated-recipe-repository.smoke
  "Smoke test for the `make-populated-recipe-repository' function."

  (mapc (lambda+ ((root-directory modes expected-name))
          (let+ ((repository (make-populated-recipe-repository
                              root-directory modes))
                 ((&labels modes (mode)
                    (list* (name mode)
                           (when-let ((parent (parent mode)))
                             (modes parent))))))
            (is (equal expected-name  (name repository)))
            (is (equal root-directory (root-directory repository)))
            (is (equal modes          (modes (mode repository))))))
        `((,#P"/root/" ("mode")                     "root")
          (,#P"/root/" ("mode" "parent1")           "root")
          (,#P"/root/" ("mode" "parent1" "parent2") "root"))))

(test setf-recipe-directory.smoke
  "Smoke test for the (setf recipe-directory) generic function."

  (let ((repository (make-recipe-repository #P"/root/" "toolkit")))
    (setf (recipe-directory :foo repository) #P"bar/")
    (is (equal #P"/root/bar/" (recipe-directory :foo repository)))

    (signals error (setf (recipe-directory :baz repository) #P"fez.txt"))))

(test recipe-path.smoke
  "Smoke test for the `recipe-path' generic function."

  (let ((repository (make-populated-recipe-repository #P"/root/" "ci")))
    (mapc
     (lambda+ ((kind name expected))
       (flet ((do-it ()
                (recipe-path repository kind name)))
         (case expected
           (error (signals error (do-it)))
           (t     (is (equal expected (do-it)))))))

     `((:no-such-kind "foo"           error)

       (:template     "foo"           ,#P"/root/templates/ci/foo.template")
       (:template     ,#P"foo"        ,#P"/root/templates/ci/foo.template")
       (:template     ,#P"foo.bar"    ,#P"/root/templates/ci/foo.bar")

       (:project      "foo"           ,#P"/root/projects/foo.project")

       (:distribution "foo"           ,#P"/root/distributions/foo.distribution")
       (:distribution "stack/foo"     ,#P"/root/distributions/stack/foo.distribution")

       (:person       :wild           ,#P"/root/persons/*.person")
       (:person       :wild-inferiors ,#P"/root/persons/**/*.person")))))

(test recipe-name.smoke
  "Smoke test for the `recipe-name' generic function."

  ;; We use the `make-mock-repository-stack' for convenience - a
  ;; `recipe-repository' instance instead of a `mock-repository'
  ;; instance would work for this test.
  (let ((repository (make-mock-repository-stack
                     '("ci" "_common")
                     '("/a/" ()
                       ("/b/" ()
                        ("/c/" ()))))))
    (mapc
     (lambda+ ((kind pathname expected-name &optional expected-repository))
       (flet ((do-it ()
                (recipe-name repository kind pathname)))
         (case expected-name
           (error (signals error (do-it)))
           (t     (let+ (((&values name repository) (do-it)))
                    (is (equal expected-name name))
                    (if (null expected-repository)
                        (is (eq nil repository))
                        (is (equal expected-repository (name repository)))))))))

     `((:no-such-kind #P"/a/foo.no-such-kind"                     error)

       (:template     #P"/a/templates/toolkit/foo.template"       nil)
       (:template     #P"/a/templates/ci/foo.template"            "foo"     "a")
       (:template     #P"/a/templates/_common/foo.template"       "foo"     "a")
       (:template     #P"/a/templates/_common/bar/foo.template"   "bar/foo" "a")
       (:template     #P"/b/templates/toolkit/foo.template"       nil)
       (:template     #P"/b/templates/ci/foo.template"            "foo"     "b")
       (:template     #P"/b/templates/_common/foo.template"       "foo"     "b")
       (:template     #P"/b/templates/_common/bar/foo.template"   "bar/foo" "b")
       (:template     #P"/c/templates/toolkit/foo.template"       nil)
       (:template     #P"/c/templates/ci/foo.template"            "foo"     "c")
       (:template     #P"/c/templates/_common/foo.template"       "foo"     "c")
       (:template     #P"/c/templates/_common/bar/foo.template"   "bar/foo" "c")

       (:distribution #P"/a/distributions/foo.distribution"       "foo"       "a")
       (:distribution #P"/a/distributions/stack/bar.distribution" "stack/bar" "a")
       (:distribution #P"/b/distributions/foo.distribution"       "foo"       "b")
       (:distribution #P"/b/distributions/stack/bar.distribution" "stack/bar" "b")
       (:distribution #P"/c/distributions/foo.distribution"       "foo"       "c")
       (:distribution #P"/c/distributions/stack/bar.distribution" "stack/bar" "c")))))

(test recipe-truename.smoke
  "Smoke test for the `recipe-truename' generic function."

  ;; We use the `mock-repository' class to avoid hitting the actual
  ;; filesystem via `cl:probe-file' and `cl:directory'.
  (let ((repository (make-mock-repository-stack
                     '("toolkit" "_common")
                     '("/a/" ("templates/toolkit/1.template"

                              "templates/_common/2.template"

                              "templates/toolkit/99.template"
                              "templates/_common/99.template")
                             ("/b/" ("templates/toolkit/3.template"

                                     "templates/_common/4.template"

                                     "templates/toolkit/99.template"
                                     "templates/_common/99.template")
                                    ("/c/" ("templates/toolkit/5.template"

                                            "templates/_common/6.template"

                                            "templates/toolkit/99.template"
                                            "templates/_common/99.template")))))))
    (mapc (lambda+ ((name expected))
            (flet ((do-it ()
                     (recipe-truename repository :template (pathname name))))
              (case expected
                (error
                 (signals recipe-not-found-error
                   (do-it))
                 (handler-case (do-it)
                   (recipe-not-found-error (condition)
                     (is (eq repository (repository condition))))))
                (t
                 (is (equal (pathname expected) (do-it)))))))
          '(("no-such.template" error)

            ("1.template"       "/a/templates/toolkit/1.template")
            ("2.template"       "/a/templates/_common/2.template")
            ("3.template"       "/b/templates/toolkit/3.template")
            ("4.template"       "/b/templates/_common/4.template")
            ("5.template"       "/c/templates/toolkit/5.template")
            ("6.template"       "/c/templates/_common/6.template")

            ("99.template"      "/a/templates/toolkit/99.template")))))

(test recipe-truenames.smoke
  "Smoke test for the `recipe-truenames' generic function."

  ;; We use the `mock-repository' class to avoid hitting the actual
  ;; filesystem via `cl:probe-file' and `cl:directory'.
  (let ((repository (make-mock-repository-stack
                     '("toolkit" "_common")
                     '("/a/" ("templates/toolkit/1.template"

                              "templates/_common/2.template"

                              "templates/toolkit/99.template"
                              "templates/_common/99.template")
                             ("/b/" ("templates/toolkit/3.template"

                                     "templates/_common/4.template"

                                     "templates/toolkit/99.template"
                                     "templates/_common/99.template")
                                    ("/c/" ("templates/toolkit/5.template"

                                            "templates/_common/6.template"

                                            "templates/toolkit/99.template"
                                            "templates/_common/99.template")))))))
    (mapc (lambda+ ((name expected))
            (let ((pathname (pathname name)))
              (is (equal expected
                         (recipe-truenames repository :template pathname)))))
          `(("no-such.template" ())

            ("1.template"       ((:probe ,#P"/a/templates/toolkit/1.template")))
            ("2.template"       ((:probe ,#P"/a/templates/_common/2.template")))
            ("3.template"       ((:probe ,#P"/b/templates/toolkit/3.template")))
            ("4.template"       ((:probe ,#P"/b/templates/_common/4.template")))
            ("5.template"       ((:probe ,#P"/c/templates/toolkit/5.template")))
            ("6.template"       ((:probe ,#P"/c/templates/_common/6.template")))

            ("99.template"      ((:probe ,#P "/a/templates/toolkit/99.template")
                                 (:probe ,#P "/a/templates/_common/99.template")
                                 (:probe ,#P "/b/templates/toolkit/99.template")
                                 (:probe ,#P "/b/templates/_common/99.template")
                                 (:probe ,#P "/c/templates/toolkit/99.template")
                                 (:probe ,#P "/c/templates/_common/99.template")))))))
