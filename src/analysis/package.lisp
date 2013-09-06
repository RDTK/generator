;;;; package.lisp --- Package definition for the analysis module.
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
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
   #:print-items)

  ;; Versions
  (:export
   #:parse-version
   #:print-version
   #:version=
   #:version<
   #:version>=
   #:version-matches)

  ;; Conditions
  (:export
   #:analyze)

  (:documentation
   "TODO"))
