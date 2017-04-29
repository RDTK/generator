;;;; package.lisp --- Package definition for aspects module.
;;;;
;;;; Copyright (C) 2015, 2016, 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:jenkins.model
  (:use
   #:cl
   #:alexandria
   #:split-sequence
   #:iterate
   #:let-plus
   #:more-conditions

   #:jenkins.model.variables)

  (:shadowing-import-from #:jenkins.model.variables
   #:as)

  ;; Conditions
  (:export
   #:instantiation-condition
   #:instantiation-condition-specification

   #:instantiation-error

   #:deployment-condition
   #:deployment-condition-thing

   #:deployment-error)

  ;; Name protocol and mixin class
  (:export
   #:name

   #:named-mixin)

  ;; Parent protocol and mixin class
  (:export
   #:parent

   #:parented-mixin)

  ;; Dependency protocol
  (:export
   #:direct-dependencies
   #:dependencies
   #:minimal-dependencies)

  ;; Access protocol
  (:export
   #:access
   #:check-access)

  ;; Instantiation protocol
  (:export
   #:instantiate?
   #:instantiate
   #:add-dependencies!)

  ;; Conditional instantiation protocol
  (:export
   #:conditions
   #:instantiate?

   #:conditional-mixin)

  ;; Deployment protocol
  (:export
   #:deploy
   #:deploy-dependencies)

  ;; Implementation protocol and mixin class
  (:export
   #:specification

   #:implementation-mixin)

  ;; Specification protocol
  (:export
   #:implementation
   #:implementations

   #:specification-mixin)

  (:documentation
   "The data model of the jenkins.project system."))
