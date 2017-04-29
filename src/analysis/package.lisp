;;;; package.lisp --- Package definition for the analysis module.
;;;;
;;;; Copyright (C) 2012-2017 Jan Moringen
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

   #:analysis-error

   #:dependency-condition
   #:dependency-condition-dependency

   #:unfulfilled-project-dependency-error
   #:unfulfilled-project-dependency-candidates

   #:unfulfilled-platform-dependency-error)

  ;; Analysis protocol
  (:export
   #:analyze)

  (:export
   #:current-platform
   #:installed-packages)

  (:documentation
   "Functions for analyzing various kinds of projects."))
