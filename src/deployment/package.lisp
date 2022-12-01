;;;; package.lisp --- Package definition for the deployment module.
;;;;
;;;; Copyright (C) 2018-2022 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:build-generator.deployment
  (:use
   #:cl
   #:alexandria
   #:let-plus
   #:more-conditions)

  (:local-nicknames
   (#:util    #:build-generator.util)

   (#:model   #:build-generator.model)
   (#:var     #:build-generator.model.variables)
   (#:project #:build-generator.model.project)
   (#:aspects #:build-generator.model.aspects))

  ;; Conditions
  (:export
   #:deployment-condition
   #:thing
   #:target

   #:deployment-error

   #:project-deployment-error)

  ;; Deployment protocol
  (:export
   #:deploy
   #:deploy-dependencies)

  ;; Target service
  (:export
   #:target                         ; service name

   #:make-target)

  ;; String utilities
  (:export
   #:job-full-name

   #:print-heading)

  ;; `command-mixin'
  (:export
   #:command-mixin

   #:command))
