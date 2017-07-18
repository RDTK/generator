;;;; package.lisp --- Package definition for the commands module.
;;;;
;;;; Copyright (C) 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:jenkins.project.commands
  (:use
   #:cl
   #:alexandria
   #:let-plus
   #:more-conditions

   #:jenkins.model
   #:jenkins.model.variables
   #:jenkins.model.project)

  (:shadow
   #:phase

   #:value)

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
