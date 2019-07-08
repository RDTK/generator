;;;; package.lisp --- Package definition for the report module.
;;;;
;;;; Copyright (C) 2015, 2016, 2017, 2018, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:build-generator.report
  (:use
   #:cl
   #:alexandria
   #:iterate
   #:let-plus
   #:more-conditions)

  (:local-nicknames
   (#:util    #:build-generator.util)

   (#:model   #:build-generator.model)
   (#:var     #:build-generator.model.variables)
   (#:project #:build-generator.model.project))

  ;; Report protocol
  (:export
   #:report)

  (:documentation
   "This package contains functionality for reporting information
    gathered from recipes merged with analysis results."))
