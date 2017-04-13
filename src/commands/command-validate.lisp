;;;; command-validate.lisp --- Validate a recipe repository.
;;;;
;;;; Copyright (C) 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.project.commands)

(define-constant +validation-levels+
    #(:syntax :instantiate :analyze :check-access)
  :test #'equalp)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun validation-level-name->index (level)
    (or (position level +validation-levels+)
        (error "~@<Unknown validation level: ~S.~@:>" level))))

(define-compiler-macro validation-level-name->index (&whole form level)
  (declare (notinline validation-level-name->index))
  (if (constantp level)
      (validation-level-name->index (eval level))
      form))

(deftype validation-level ()
  `(member ,@(coerce +validation-levels+ 'list)))

(defun validation-level>= (left right)
  (>= (validation-level-name->index left)
      (validation-level-name->index right)))

(define-compiler-macro validation-level>= (left right)
  `(>= (validation-level-name->index ,left)
       (validation-level-name->index ,right)))

(defclass validate ()
  ((recipe-directory :initarg  :recipe-directory
                     :type     pathname
                     :reader   recipe-directory
                     :documentation
                     "Directory from which recipes should be collected.")
   (validation-level :initarg  :validation-level
                     :type     validation-level
                     :reader   validation-level
                     :initform :syntax
                     :documentation
                     #.(format nil "Extent of the validation to be performed.~@
                        ~@
                        Each level includes the validation actions of ~
                        the previous level.~@
                        ~@
                        syntax~@
                        instantiate~@
                        analyze~@
                        check-access.")))
  (:documentation
   "Perform basic sanity checks for a given recipe repository."))

(service-provider:register-provider/class
 'command :validate :class 'validate)

(jenkins.project.commandline-options:define-option-mapping
    (*command-schema* "validate")
  (0 "recipe-directory" "DIRECTORY" t)
  (1 "validation-level" "LEVEL"))

(defmethod command-execute ((command validate))
  (let+ ((level              (validation-level command))
         (pattern            (merge-pathnames
                              "distributions/*.distribution"
                              (uiop:ensure-directory-pathname
                               (recipe-directory command))))
         (distribution-files (directory pattern))
         ((&values distributions projects)
          (generate-load distribution-files "toolkit" '()
                         :generator-version (generator-version)))
         ((&values distributions &ign)
          (when (validation-level>= level :analyze)
            (generate-analyze distributions projects
                              :generator-version (generator-version)
                              :cache-directory   *cache-directory*
                              :temp-directory    *temp-directory*))))
    (when (validation-level>= level :check-access)
      (check-distribution-access distributions))))
