;;;; conditions.lisp --- Conditions used by the deployment module.
;;;;
;;;; Copyright (C) 2012-2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:build-generator.deployment)

(define-condition deployment-condition (chainable-condition)
  ((thing  :initarg  :thing
           :reader   thing
           :documentation
           "The thing the deployment of which caused the condition.")
   (target :initarg  :target
           :reader   target
           :documentation
           "The target of the deployment which caused the condition."))
  (:default-initargs
   :thing  (missing-required-initarg 'deployment-condition :thing)
   :target (missing-required-initarg 'deployment-condition :target))
  (:documentation
   "Subclasses of this condition are signaled to indicate certain
    conditions during the deployment of things."))

(define-condition deployment-error (error
                                    deployment-condition)
  ()
  (:report
   (lambda (condition stream)
     (format stream "~@<Error during deployment of ~
                     ~A to ~A.~/more-conditions:maybe-print-cause/~@:>"
             (thing condition) (target condition) condition)))
  (:documentation
   "This error is signaled when an error is encountered during
    deployment of a thing."))

(define-condition project-deployment-error (deployment-error)
  ()
  (:report
   (lambda (condition stream)
     (format stream "~@<Error during deployment of project version ~
                     ~/print-items:format-print-items/ to ~A.~
                     ~/more-conditions:maybe-print-cause/~@:>"
             (print-items:print-items (thing condition))
             (target condition)
             condition))))
