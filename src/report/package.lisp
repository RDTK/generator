;;;; package.lisp --- Package definition for the report module.
;;;;
;;;; Copyright (C) 2015, 2016, 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:jenkins.report
  (:use
   #:cl
   #:alexandria
   #:split-sequence
   #:iterate
   #:let-plus
   #:more-conditions
   #:print-items

   #:jenkins.model
   #:jenkins.model.project)

  (:shadowing-import-from #:jenkins.model.variables
   #:value #:value/cast

   #:as

   #:variables)

  ;; Report protocol
  (:export
   #:report)

  (:documentation
   "This package contains functionality for reporting information
    gathered from recipes merged with analysis results."))
