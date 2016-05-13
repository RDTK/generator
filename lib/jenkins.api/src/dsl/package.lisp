;;;; package.lisp --- Package definition for dsl module.
;;;;
;;;; Copyright (C) 2012, 2013, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:jenkins.dsl
  (:use
   #:cl
   #:alexandria
   #:split-sequence
   #:iterate
   #:let-plus
   #:more-conditions

   #:jenkins.api)

  (:shadow
   #:job
   #:parameters)

  (:export
   #:job
   #:parameters)

  (:documentation
   "TODO"))
