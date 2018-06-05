;;;; command-validate.lisp --- Validate a recipe repository.
;;;;
;;;; Copyright (C) 2017, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.project.commands)

(defclass validate ()
  ((recipes :initarg  :recipes
            :type     pathname
            :reader   recipes
            :documentation
            "Distribution recipe or root directory of recipe repository."))
  (:documentation
   "Perform basic sanity checks for a given recipe repository."))

(service-provider:register-provider/class
 'command :validate :class 'validate)

(jenkins.project.commandline-options:define-option-mapping
    (*command-schema* "validate")
  (0 "recipes" "FILENAME-OR-DIRECTORY" t))

(defmethod command-execute ((command validate))
  (let* ((recipes            (recipes command))
         (distribution-files
           (cond
             ((wild-pathname-p recipes)
              (directory recipes))
             ((equal (pathname-type recipes) "distribution")
              (list recipes))
             (t
              (directory
               (merge-pathnames
                "distributions/*.distribution"
                (uiop:ensure-directory-pathname recipes)))))))
    (generate-load distribution-files "toolkit" '()
                   :generator-version (generator-version))))
