;;;; conditions.lisp --- Conditions signaled by the report module.
;;;;
;;;; Copyright (C) 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.report)

(define-condition report-condition (chainable-condition)
  ((style  :initarg :style
           :reader  style
           :documentation
           "The style of the report for which the condition is
            signaled.")
   (object :initarg :object
           :reader  object
           :documentation
           "The object for which the report was being produced."))
  (:default-initargs
   :style  (missing-required-initarg 'report-condition :style)
   :object (missing-required-initarg 'report-condition :object)))

(define-condition report-error (report-condition
                                error)
  ()
  (:report
   (lambda (condition stream)
     (let+ (((&accessors-r/o style object) condition))
       (format stream "~@<Error generating ~A report for ~
                       ~/print-items:format-print-items/~
                       ~/more-conditions:maybe-print-cause/~@:>"
               style (print-items:print-items object) condition)))))
