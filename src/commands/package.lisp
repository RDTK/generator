;;;; package.lisp --- Package definition for the commands module.
;;;;
;;;; Copyright (C) 2017, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:jenkins.project.commands
  (:use
   #:cl
   #:alexandria
   #:let-plus
   #:iterate
   #:more-conditions)

  (:local-nicknames
   (#:model   #:jenkins.model)
   (#:var     #:jenkins.model.variables)
   (#:project #:jenkins.model.project)
   (#:aspects #:jenkins.model.aspects)

   (#:steps   #:jenkins.project.steps))

  (:shadow
   #:generate
   #:phase)

  (:shadowing-import-from #:jenkins.model.variables
   #:as)

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
