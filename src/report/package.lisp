;;;; package.lisp --- Package definition for the report module.
;;;;
;;;; Copyright (C) 2015, 2016 Jan Moringen
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

   #:jenkins.project)

  (:shadowing-import-from #:jenkins.project
   #:as)

  ;; Report protocol
  (:export
   #:report)

  (:documentation
   "This package contains functionality for reporting information
    gathered from recipes merged with analysis results."))
