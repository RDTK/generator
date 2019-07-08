;;;; conditions.lisp --- Conditions used in the model.variables module.
;;;;
;;;; Copyright (C) 2012-2017, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:build-generator.model.variables)

;;; Evaluation-related conditions

(define-condition expression-cycle-error (error)
  ((path :initarg :path
         :type    list
         :reader  expression-cycle-error-path))
  (:report
   (lambda (condition stream)
     (format stream "~@<The following expression evaluation lead to a ~
                     cycle:~
                     ~@:_~@:_    ~{~{~24A in ~A~}~^~@:_ -> ~}~
                     ~@:_~@:_.~@:>"
             (expression-cycle-error-path condition))))
  (:default-initargs
   :path (missing-required-initarg 'expression-cycle-error :path))
  (:documentation
   "This error is signaled when a cycle is detected during variable
    expansion."))

;;; Variable schema conditions

(define-condition variable-condition (condition)
  ((name :initarg :name
         :reader  variable-condition-name
         :documentation
         "Stores the name of the variable."))
  (:default-initargs
   :name (missing-required-initarg 'variable-condition :name))
  (:documentation
   "Superclass for variable-related condition classes."))

(define-condition unused-variable-warning (variable-condition
                                           style-warning)
  ()
  (:report
   (lambda (condition stream)
     (format stream "~@<Variable ~S is defined but never used.~@:>"
             (variable-condition-name condition))))

  (:documentation
   "Signaled when an access to an undefined variable is detected at
    compile-time."))

(define-condition undefined-variable-condition (variable-condition)
  ()
  (:report
   (lambda (condition stream)
     (format stream "~@<Access to undefined variable ~S.~@:>"
             (variable-condition-name condition))))
  (:documentation
   "Superclass for condition classes related to access to undefined
    variables."))

(define-condition undefined-variable-warning (undefined-variable-condition
                                              style-warning)
  ()
  (:documentation
   "Signaled when an access to an undefined variable is detected at
    compile-time."))

(define-condition undefined-variable-error (undefined-variable-condition
                                            error)
  ()
  (:documentation
   "Signaled when an access to an undefined variable occurs at
    runtime."))
