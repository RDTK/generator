;;;; conditions.lisp --- Conditions used in the analysis module.
;;;;
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.analysis)

(define-condition analysis-condition (chainable-condition)
  ((specification :initarg  :specification
                  :reader   analysis-condition-specification
                  :documentation
                  "Stores the specification, the analysis resulted in
                   the condition being signaled."))
  (:default-initargs
   :specification (missing-required-initarg 'analysis-condition :specification))
  (:documentation
   "Subclasses of this condition are signaled to indicate certain
    condition during the analysis of specifications."))

(define-condition analysis-error (error
                                  analysis-condition)
  ()
  (:report
   (lambda (condition stream)
     (format stream "~@<Error during analysis of ~A~
                     ~/more-conditions:maybe-print-cause/~@:>"
             (analysis-condition-specification condition)
             condition)))
  (:documentation
   "This error is signaled when an error is encountered during the
    analysis of a specification."))
