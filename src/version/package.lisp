;;;; package.lisp --- Package definition for the version module.
;;;;
;;;; Copyright (C) 2014, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:build-generator.version
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
