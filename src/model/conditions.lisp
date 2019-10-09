;;;; conditions.lisp --- Conditions used by the project module.
;;;;
;;;; Copyright (C) 2012-2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:build-generator.model)

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
