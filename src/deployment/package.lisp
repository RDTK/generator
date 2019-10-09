;;;; package.lisp --- Package definition for the deployment module.
;;;;
;;;; Copyright (C) 2018, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:build-generator.deployment
  (:use
   #:cl
   #:let-plus
   #:more-conditions)

  ;; Conditions
  (:export
   #:deployment-condition
   #:thing

   #:deployment-error

   #:project-deployment-error)

  ;; Deployment protocol
  (:export
   #:deploy
   #:deploy-dependencies))
