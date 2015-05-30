;;;; package.lisp --- Package definition for project module.
;;;;
;;;; Copyright (C) 2012, 2013, 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:jenkins.project
  (:use
   #:cl
   #:alexandria
   #:split-sequence
   #:iterate
   #:let-plus
   #:more-conditions
   #:print-items

   #:jenkins.api
   #:jenkins.dsl)

  (:shadow
   #:node)

  (:shadowing-import-from #:jenkins.dsl
   #:job)

  (:import-from #:jenkins.version
   #:parse-version
   #:version>=
   #:version-matches)

  ;; Conditions
  (:export
   #:instantiation-condition
   #:instantiation-condition-specification

   #:instantiation-error

   #:deployment-condition
   #:deployment-condition-thing

   #:deployment-error)

  ;; Template protocol
  (:export
   #:find-template
   )

  ;; Project specification protocol
  (:export
   #:templates
   #:versions
   #:jobs)

  ;; Version specification protocol and class
  (:export
   #:version-spec)

  ;; Job specification protocol
  (:export
   #:tags)

  ;; Template specification protocol
  (:export
   #:inherit
   #:aspects
   #:jobs)

  ;; Aspect specification protocol
  (:export
   #:aspects
   #:filter)

  ;; Project protocol
  (:export
   #:find-project)

  ;; Instance protocol
  (:export
   #:find-instance)

  ;; Provider registry
  (:export
   #:find-provider
   #:find-provider/version
   #:providers/alist)

  ;; Platform requirements protocol
  (:export
   #:platform-requires)

  ;; Access protocol
  (:export
   #:access
   #:check-access)

  ;; Instantiation protocol
  (:export
   #:instantiate?
   #:instantiate
   #:add-dependencies!)

  ;; Deployment protocol
  (:export
   #:deploy
   #:deploy-dependencies)

  ;; Implementation protocol
  (:export
   #:specification)

  ;; Specification protocol
  (:export
   #:implementation
   #:implementations)

  ;; Name protocol
  (:export
   #:name)

  ;; Dependency protocol
  (:export
   #:direct-dependencies
   #:dependencies)

  ;; Variable protocol
  (:export
   #:direct-variables
   #:variables
   #:lookup
   #:value)

  ;; JSON stuff
  (:export
   #:load-template/json
   #:load-project-spec/json
   #:load-distribution/json)

  (:documentation
   "TODO"))
