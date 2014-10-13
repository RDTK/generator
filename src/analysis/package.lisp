;;;; package.lisp --- Package definition for the analysis module.
;;;;
;;;; Copyright (C) 2012, 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:jenkins.analysis
  (:use
   #:cl
   #:alexandria
   #:split-sequence
   #:iterate
   #:let-plus
   #:more-conditions
   #:print-items

   #:jenkins.version)

  ;; Conditions
  (:export
   #:analysis-condition
   #:analysis-condition-specification

   #:analysis-error)

  ;; Analysis protocol
  (:export
   #:analyze)

  (:export
   #:current-platform
   #:installed-packages)

  (:documentation
   "TODO"))
