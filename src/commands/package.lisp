;;;; package.lisp --- Package definition for the commands module.
;;;;
;;;; Copyright (C) 2017, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:build-generator.commands
  (:use
   #:cl
   #:alexandria
   #:let-plus
   #:iterate
   #:more-conditions)

  (:local-nicknames
   (#:util     #:build-generator.util)

   (#:analysis #:build-generator.analysis)

   (#:model    #:build-generator.model)
   (#:var      #:build-generator.model.variables)
   (#:project  #:build-generator.model.project)
   (#:aspects  #:build-generator.model.aspects)

   (#:steps    #:build-generator.steps))

  (:shadow
   #:phase)

  ;; Conditions
  (:export
   #:command-condition
   #:command

   #:command-configuration-problem

   #:command-not-found-error

   #:option-configuration-problem

   #:option-value-error
   #:value)

  ;; Command protocol
  (:export
   #:command-execute
   #:make-command)

  ;; High-level interface
  (:export
   #:configure-command
   #:execute-command)

  (:documentation
   "Command classes implementing user-level blocks of functionality."))
