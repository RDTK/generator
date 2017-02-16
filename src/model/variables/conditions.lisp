;;;; conditions.lisp --- Conditions used in the model.variables module.
;;;;
;;;; Copyright (C) 2012-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.model.variables)

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
