;;;; package.lisp --- Package definition for aspects module.
;;;;
;;;; Copyright (C) 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:jenkins.model.aspects
  (:use
   #:cl
   #:alexandria
   #:split-sequence
   #:iterate
   #:let-plus
   #:more-conditions
   #:print-items

   #:jenkins.model
   #:jenkins.model.variables

   #:jenkins.api
   #:jenkins.dsl)

  (:shadowing-import-from #:jenkins.model ; TODO hack
   #:name

   #:sort-with-partial-order)

  (:shadowing-import-from #:jenkins.model.variables ; TODO hack
   #:as)

  (:shadowing-import-from #:jenkins.api ; TODO hack
   #:parameters)

  (:shadowing-import-from #:jenkins.dsl ; TODO hack
   #:job)

  ;; Aspect protocol
  (:export
   #:aspect<

   #:extend!)

  ;; Aspect container protocol
  (:export
   #:aspects)

  (:documentation
   "Aspects extend the target being generated with behavior fragments."))
