;;;; command-validate.lisp --- Validate a recipe repository.
;;;;
;;;; Copyright (C) 2017, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.project.commands)

(defclass validate ()
  ((recipe-directory :initarg  :recipe-directory
                     :type     pathname
                     :reader   recipe-directory
                     :documentation
                     "Directory from which recipes should be collected."))
  (:documentation
   "Perform basic sanity checks for a given recipe repository."))

(service-provider:register-provider/class
 'command :validate :class 'validate)

(jenkins.project.commandline-options:define-option-mapping
    (*command-schema* "validate")
  (0 "recipe-directory" "DIRECTORY" t))

(defmethod command-execute ((command validate))
  (let* ((pattern            (merge-pathnames
                              "distributions/*.distribution"
                              (uiop:ensure-directory-pathname
                               (recipe-directory command))))
         (distribution-files (directory pattern)))
    (generate-load distribution-files "toolkit" '()
                   :generator-version (generator-version))))
