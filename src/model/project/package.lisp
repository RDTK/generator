;;;; package.lisp --- Package definition for project module.
;;;;
;;;; Copyright (C) 2012-2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:build-generator.model.project
  (:use
   #:cl
   #:alexandria
   #:split-sequence
   #:iterate
   #:let-plus
   #:more-conditions)

  (:local-nicknames
   (#:bp       #:architecture.builder-protocol)

   (#:util     #:build-generator.util)

   (#:analysis #:build-generator.analysis)

   (#:model    #:build-generator.model)
   (#:var      #:build-generator.model.variables)
   (#:aspects  #:build-generator.model.aspects))

  (:shadow
   #:node)

  (:import-from #:build-generator.version
   #:parse-version
   #:version>=
   #:version-matches)

  ;; People
  (:export
   #:all-persons
   #:ensure-persons!)

  ;; Template protocol
  (:export
   #:find-template)

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
   #:direct-requires #:requires #:requires-of-kind
   #:direct-provides #:provides #:provides-of-kind

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

  ;; Mode protocol
  (:export
   #:name
   #:parent

   #:mode                           ; class
   #:ensure-mode)

  ;; Recipe repository protocol
  (:export
   #:root-directory
   #:mode                           ; accessor

   #:recipe-directory

   #:recipe-path
   #:recipe-truename
   #:recipe-truenames

   #:recipe-name

   #:populate-recipe-repository!

   #:recipe-repository
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
