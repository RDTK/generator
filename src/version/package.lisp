;;;; package.lisp --- Package definition for the version module.
;;;;
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:jenkins.version
  (:use
   #:cl
   #:alexandria
   #:split-sequence
   #:iterate
   #:let-plus)

  ;; Versions
  (:export
   #:parse-version
   #:print-version
   #:version=
   #:version<
   #:version>=
   #:version-matches)

  (:documentation
   "This package contains version-related functions."))
