;;;; package.lisp --- Package definition for the deployment.github-actions module.
;;;;
;;;; Copyright (C) 2018, 2019, 2020 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:build-generator.deployment.github-actions
  (:use
   #:cl
   #:alexandria
   #:let-plus
   #:more-conditions)

  (:shadow
   #:directory
   #:step)

  (:local-nicknames
   (#:util    #:build-generator.util)

   (#:model   #:build-generator.model)
   (#:var     #:build-generator.model.variables)
   (#:project #:build-generator.model.project)
   (#:aspects #:build-generator.model.aspects)

   (#:deploy  #:build-generator.deployment)))
