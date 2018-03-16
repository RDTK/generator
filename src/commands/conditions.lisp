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

;;; Phase-related conditions

(define-condition phase-condition (condition)
  ((phase :initarg :phase
          :type    keyword
          :reader  phase
          :documentation
          "Stores the execution phase during which the condition was
           signaled."))
  (:default-initargs
   :phase (missing-required-initarg 'phase-condition :phase))
  (:documentation
   "Superclass for phase-related conditions."))

(define-condition deferred-problem-condition (condition)
  ((conditions :initarg :conditions
               :type     list
               :reader   conditions
               :documentation
               "Stores the conditions that were deferred during the
                execution of a particular phase."))
  (:default-initargs
   :problems (missing-required-initarg 'deferred-problem-condition :conditions))
  (:documentation
   "Superclass for condition classes which collect deferred
    conditions."))

(define-condition phase-error (error
                               phase-condition)
  ()
  (:documentation
   "Superclass for phase error conditions."))

(define-condition deferred-phase-error (phase-error
                                        deferred-problem-condition)
  ()
  (:report
   (lambda (condition stream)
     (let+ (((&accessors-r/o phase conditions) condition))
       (format stream "~@<~D problem~:P during ~A phase:~@:_~@:_~
                       ~2@T~@<~
                         ~{~
                           ~<~A:~
                             ~@:_~2@T~<~A~:>~
                           ~:>~
                           ~^~@:_~@:_~
                         ~}~
                       ~:>~@:>"
               (length conditions) phase
               (mapcar (lambda (condition)
                         (list (type-of condition) condition))
                       conditions)))))
  (:documentation
   "Signaled when deferred errors have accumulated at the end of a
    phase."))

(defun deferred-phase-cerror (phase conditions)
  "Signal a continuable `deferred-phase-error'."
  (with-simple-restart (continue "~@<Ignore problems in phase ~A and ~
                                  continue.~@:>"
                                 phase)
    (error 'deferred-phase-error :phase phase :conditions conditions)))
