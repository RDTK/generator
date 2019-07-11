;;;; recipe-repository.lisp --- Unit tests for the recipe repository.
;;;;
;;;; Copyright (C) 2019 Jan Moringen
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

(defun make-populated-mock-repository (root-directory mode-or-modes files)
  (populate-recipe-repository!
   (make-instance 'mock-repository
                  :root-directory (pathname root-directory)
                  :mode           (ensure-mode mode-or-modes)
                  :pathnames      (map 'list (rcurry #'merge-pathnames root-directory)
                                       files))))

;;; Tests

(def-suite* :build-generator.model.project.concrete-syntax.recipe-repository
  :in :build-generator.model.project)

(test make-populated-recipe-repository.smoke
  "Smoke test for the `make-populated-recipe-repository' function."

  (mapc (lambda+ ((root-directory &rest modes))
          (let+ ((repository (apply #'make-populated-recipe-repository
                                    root-directory modes))
                 ((&labels modes (mode)
                    (list* (name mode)
                           (when-let ((parent (parent mode)))
                             (modes parent))))))
            (is (equal root-directory (root-directory repository)))
            (is (equal modes          (modes (mode repository))))))
        `((,#P"/root/" "mode")
          (,#P"/root/" "mode" "parent1")
          (,#P"/root/" "mode" "parent1" "parent2"))))

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

     `((:no-such-kind "foo"        error)

       (:template     "foo"        ,#P"/root/templates/ci/foo.template")
       (:template     ,#P"foo"     ,#P"/root/templates/ci/foo.template")
       (:template     ,#P"foo.bar" ,#P"/root/templates/ci/foo.bar")

       (:project      "foo"        ,#P"/root/projects/foo.project")

       (:distribution "foo"        ,#P"/root/distributions/foo.distribution")
       (:distribution "stack/foo"  ,#P"/root/distributions/stack/foo.distribution")

       (:person       :wild        ,#P"/root/persons/*.person")))))

(test recipe-name.smoke
  "Smoke test for the `recipe-name' generic function."

  (let ((repository (make-populated-recipe-repository #P"/root/" "ci" "_common")))
    (mapc
     (lambda+ ((kind pathname expected))
       (flet ((do-it ()
                (recipe-name repository kind pathname)))
         (case expected
           (error (signals error (do-it)))
           (t     (is (equal expected (do-it)))))))

     `((:no-such-kind #P"/root/foo.no-such-kind"                     error)

       (:template     #P"/root/templates/toolkit/foo.template"       nil)
       (:template     #P"/root/templates/ci/foo.template"            "foo")
       (:template     #P"/root/templates/_common/foo.template"       "foo")
       (:template     #P"/root/templates/_common/bar/foo.template"   "bar/foo")

       (:distribution #P"/root/distributions/foo.distribution"       "foo")
       (:distribution #P"/root/distributions/stack/bar.distribution" "stack/bar")))))

(test recipe-truename.smoke
  "Smoke test for the `recipe-truename' generic function."

  ;; We use the `mock-repository' class to avoid hitting the actual
  ;; filesystem via `cl:probe-file' and `cl:directory'.
  (let ((repository (make-populated-mock-repository
                     "/a/" '("toolkit" "_common")
                     '("templates/toolkit/1.template"

                       "templates/_common/2.template"

                       "templates/toolkit/99.template"
                       "templates/_common/99.template"))))
    (mapc
     (lambda+ ((name expected))
       (flet ((do-it ()
                (recipe-truename repository :template (pathname name))))
         (case expected
           (error
            (signals recipe-not-found-error
              (do-it))
            (handler-case (do-it)
              (recipe-not-found-error (condition)
                (is (eq repository (repository condition))))))
           (t     (is (equal (pathname expected) (do-it)))))))
     '(("no-such.template" error)

       ("1.template"       "/a/templates/toolkit/1.template")
       ("2.template"       "/a/templates/_common/2.template")
       ("99.template"      "/a/templates/toolkit/99.template")))))

(test recipe-truenames.smoke
  "Smoke test for the `recipe-truenames' generic function."

  ;; We use the `mock-repository' class to avoid hitting the actual
  ;; filesystem via `cl:probe-file' and `cl:directory'.
  (let ((repository (make-populated-mock-repository
                     "/a/" '("toolkit" "_common")
                     '("templates/toolkit/1.template"

                       "templates/_common/2.template"

                       "templates/toolkit/99.template"
                       "templates/_common/99.template"))))
    (mapc
     (lambda+ ((name expected))
       (let ((pathname (pathname name)))
         (is (equal expected
                    (recipe-truenames repository :template pathname)))))
     `(("no-such.template" ())

       ("1.template"       ((:probe ,#P"/a/templates/toolkit/1.template")))
       ("2.template"       ((:probe ,#P"/a/templates/_common/2.template")))
       ("99.template"      ((:probe ,#P "/a/templates/toolkit/99.template")
                            (:probe ,#P "/a/templates/_common/99.template")))))))
