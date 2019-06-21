;;;; recipe-repository.lisp --- Unit tests for the recipe repository.
;;;;
;;;; Copyright (C) 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.model.project.test)

(def-suite* :jenkins.project.model.project.concrete-syntax.recipe-repository
  :in :jenkins.project.model.project)

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
