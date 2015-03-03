;;;; conditions.lisp --- Conditions used in the model.aspects module.
;;;;
;;;; Copyright (C) 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.model.aspects)

(define-condition parameter-condition (condition)
  ((aspect    :initarg  :aspect
              :reader   parameter-condition-aspect
              :documentation
              "Stores the aspect to which the parameter is associated.")
   (parameter :initarg  :parameter
              :reader   parameter-condition-parameter
              :documentation
              "Stores the parameter for which the invalid value has
               been supplied."))
  (:default-initargs
   :aspect    (missing-required-initarg 'parameter-condition :aspect)
   :parameter (missing-required-initarg 'parameter-condition :parameter))
  (:documentation
   "Superclass for aspect parameter-related condition classes."))

(define-condition missing-argument-error (parameter-condition
                                          error)
  ()
  (:report
   (lambda (condition stream)
     (let* ((aspect (parameter-condition-aspect condition))
            (parameter (parameter-condition-parameter condition))
            (variable (aspect-parameter-variable parameter))
            (name (variable-info-name variable)))
       (format stream "~@<No value has been supplied for the required ~
                       ~A parameter of ~A.~@:>"
               name aspect))))
  (:documentation
   "Signaled when a required aspect parameter is not supplied."))

(defun missing-argument-error (aspect parameter)
  (error 'missing-argument-error :aspect aspect :parameter parameter))

(define-condition argument-condition (parameter-condition)
  ((value :initarg  :value
          :reader   argument-condition-value
          :documentation
          "Stores value that caused the condition to be signaled."))
  (:default-initargs
   :value (missing-required-initarg 'argument-condition :value))
  (:documentation
   "Superclass for aspect argument-related condition classes."))

(define-condition argument-type-error (argument-condition
                                       chainable-condition
                                       error)
  ()
  (:report
   (lambda (condition stream)
     (let* ((aspect    (parameter-condition-aspect      condition))
            (parameter (parameter-condition-parameter   condition))
            (value     (argument-condition-value condition))
            (variable  (aspect-parameter-variable parameter))
            (name      (variable-info-name variable))
            (type      (variable-info-type variable)))
       (format stream "~@<The value ~S which has been supplied for the ~
                       ~S parameter of ~A is not of type ~S.~
                       ~/more-conditions:maybe-print-cause/~@:>"
               value name aspect type condition))))
  (:documentation
   "Signaled when an aspect argument is not of the expected type."))
