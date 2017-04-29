;;;; package.lisp --- Package definition for project module.
;;;;
;;;; Copyright (C) 2012-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:jenkins.model.project
  (:use
   #:cl
   #:alexandria
   #:split-sequence
   #:iterate
   #:let-plus
   #:more-conditions

   #:jenkins.model
   #:jenkins.model.variables

   #:jenkins.api
   #:jenkins.dsl)

  (:shadow
   #:node)

  (:shadowing-import-from #:jenkins.api
   #:parameters)

  (:shadowing-import-from #:jenkins.dsl
   #:job)

  (:shadowing-import-from #:jenkins.model
   #:name)

  (:shadowing-import-from #:jenkins.model.variables
   #:as)

  (:import-from #:jenkins.model.aspects
   #:aspects

   #:make-aspect)

  (:import-from #:jenkins.version
   #:parse-version
   #:version>=
   #:version-matches)

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

  ;; Template specification protocol
  (:export
   #:inherit
   #:aspects
   #:jobs)

  ;; Aspect specification protocol
  (:export
   #:aspects)

  ;; Project protocol
  (:export
   #:find-project)

  ;; Provider registry
  (:export
   #:find-provider
   #:find-provider/version
   #:providers/alist)

  ;; Platform requirements protocol
  (:export
   #:platform-requires)

  ;; JSON stuff
  (:export
   #:load-template/json
   #:load-project-spec/json
   #:load-distribution/json)

  (:documentation
   "Contains distribution, project and related concepts of the model."))
