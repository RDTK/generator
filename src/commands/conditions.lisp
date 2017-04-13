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
