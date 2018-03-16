;;;; conditions.lisp --- Conditions signaled by the commands module.
;;;;
;;;; Copyright (C) 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.project.commands)

(define-condition command-condition (condition)
  ((command :initarg :command
            :type    string
            :reader  command
            :documentation
            "Stores the name of the command this condition refers
             to."))
  (:default-initargs
   :command (missing-required-initarg 'command-condition :command))
  (:documentation
   "Superclass for command-related conditions."))

(define-condition command-configuration-problem (command-condition)
  ()
  (:documentation
   "Superclass for command-related warning and error conditions."))

(define-condition command-not-found-error (command-configuration-problem
                                           error)
  ()
  (:report
   (lambda (condition stream)
     (format stream "~@<\"~A\" is not a known command.~@:>"
             (command condition))))
  (:documentation
   "Signaled when a specified command cannot be bound."))

(define-condition option-configuration-problem
    (command-configuration-problem
     jenkins.project.commandline-options:option-condition)
  ()
  (:documentation
   "Superclass for command option-relation conditions."))

(define-condition option-value-error (option-configuration-problem
                                      more-conditions:chainable-condition
                                      error)
  ((value :initarg :value
          :type    string
          :reader  value
          :documentation
          "Stores the unparsed offending value."))
  (:default-initargs
   :value (missing-required-initarg 'option-value-error :value))
  (:report
   (lambda (condition stream)
     (format stream "~@<~S is not a valid value for the \"~A\" option ~
                     of the \"~A\" command (specified via \"~A\")~
                     ~/more-conditions:maybe-print-cause/~@:>"
             (value condition)
             (jenkins.project.commandline-options:option condition)
             (command condition)
             :option-designator condition)))
  (:documentation
   "Signaled when an invalid value for a command option is encountered."))
