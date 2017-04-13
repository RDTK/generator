;;;; protocol.lisp --- Protocol provided by the commands module.
;;;;
;;;; Copyright (C) 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.project.commands)

;;; Command service

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass command-service (service-provider:standard-service
                             service-provider::change-hook-mixin)
    ()))

(service-provider:define-service command
  (:service-class command-service))

;;; Command protocol

(defgeneric command-execute (command)
  (:documentation
   "Execute the already-configured COMMAND."))

(defgeneric make-command (provider &rest args)
  (:method ((provider t) &rest args)
    (apply #'service-provider:make-provider 'command provider args))
  (:documentation
   "Make and return a command according to PROVIDER and ARGS."))

;;; Command configuration

(defvar *command-schema*
  (configuration.options.service-provider:service-schema
   (service-provider:find-service 'command)))

;;; High-level interface: find, instantiate and execute a command
