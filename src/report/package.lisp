;;;; package.lisp --- Package definition for the report module.
;;;;
;;;; Copyright (C) 2015, 2016, 2017, 2018, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:jenkins.report
  (:use
   #:cl
   #:alexandria
   #:iterate
   #:let-plus
   #:more-conditions)

  (:local-nicknames
   (#:model   #:jenkins.model)
   (#:var     #:jenkins.model.variables)
   (#:project #:jenkins.model.project))

  (:import-from #:jenkins.util
   #:safe-name)

  ;; Report protocol
  (:export
   #:report)

  (:documentation
   "This package contains functionality for reporting information
    gathered from recipes merged with analysis results."))
