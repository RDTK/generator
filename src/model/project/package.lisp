;;;; package.lisp --- Package definition for project module.
;;;;
;;;; Copyright (C) 2012-2019 Jan Moringen
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

   #:jenkins.api
   #:jenkins.dsl)

  (:local-nicknames
   (#:bp      #:architecture.builder-protocol)

   (#:model   #:jenkins.model)
   (#:var     #:jenkins.model.variables)
   (#:aspects #:jenkins.model.aspects))

  (:shadow
   #:node)

  (:shadowing-import-from #:jenkins.api
   #:parameters)

  (:shadowing-import-from #:jenkins.dsl
   #:job)

  (:import-from #:jenkins.version
   #:parse-version
   #:version>=
   #:version-matches)

  ;; People
  (:export
   #:all-persons
   #:ensure-persons!)

  ;; Template protocol
  (:export
   #:find-template
   )

  ;; Distribution specification protocol
  (:export
   #:direct-includes
   #:direct-versions
   #:versions)

  ;; Project include protocol and class
  (:export
   #:distribution-include

   #:distribution)

  ;; Project include protocol and class
  (:export
   #:project-include

   #:project
   #:version)

  ;; Resolved project include protocol and class
  (:export
   #:resolved-project-include

   #:version)

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
   #:jobs)

  ;; Project protocol
  (:export
   #:find-project)

  ;; Version protocol
  (:export
   #:context)

  ;; Include context protocol
  (:export
   #:distribution)

  ;; Provider registry
  (:export
   #:find-provider/version)

  ;; Requires/provides protocol
  (:export
   #:requires
   #:requires-of-kind
   #:provides
   #:provides-of-kind

   #:direct-dependencies/reasons)

  ;; Person container protocol
  (:export
   #:persons
   #:persons-in-roles/plist
   #:persons-in-role)

  ;; Platform requirements protocol
  (:export
   #:platform-requires
   #:platform-provides)

  ;; Recipe repository protocol
  (:export
   #:root-directory
   #:recipe-directory
   #:recipe-path

   #:populate-recipe-repository!

   #:make-recipe-repository
   #:make-populated-recipe-repository)

  ;; YAML stuff
  (:export
   #:load-person/yaml
   #:load-template/yaml
   #:load-project-spec/yaml
   #:load-distribution/yaml)

  (:documentation
   "Contains distribution, project and related concepts of the model."))
