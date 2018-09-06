;;;; conditions.lisp --- Conditions used by the project module.
;;;;
;;;; Copyright (C) 2012-2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.model)

;;; Instantiation-related conditions

(define-condition instantiation-condition (chainable-condition)
  ((specification :initarg  :specification
                  :reader   instantiation-condition-specification
                  :documentation
                  "The specification the instantiation of which caused
                   the condition."))
  (:default-initargs
   :specification (missing-required-initarg
                   'instantiation-condition :specification))
  (:documentation
   "Subclasses of this condition are signaled to indicate certain
    condition during the instantiation of specifications."))

(define-condition instantiation-error (error
                                       instantiation-condition)
  ()
  (:report
   (lambda (condition stream)
     (format stream "~@<Error during instantiation of ~
                     ~A~/more-conditions:maybe-print-cause/~@:>"
             (instantiation-condition-specification condition)
             condition)))
  (:documentation
   "This error is signaled when an error is encountered during the
    instantiation of a specification."))

;;; Deployment-related conditions

(define-condition deployment-condition (chainable-condition)
  ((thing :initarg  :thing
          :reader   deployment-condition-thing
          :documentation
          "The thing the deployment of which caused the condition."))
  (:default-initargs
   :thing (missing-required-initarg 'deployment-condition :thing))
  (:documentation
   "Subclasses of this condition are signaled to indicate certain
    conditions during the deployment of things."))

(define-condition deployment-error (error
                                    deployment-condition)
  ()
  (:report
   (lambda (condition stream)
     (format stream "~@<Error during deployment of ~
                     ~A~/more-conditions:maybe-print-cause/~@:>"
             (deployment-condition-thing condition)
             condition)))
  (:documentation
   "This error is signaled when an error is encountered during
    deployment of a thing."))
