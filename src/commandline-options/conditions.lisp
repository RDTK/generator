;;;; conditions.lisp --- Conditions signaled by the commandline-options module.
;;;;
;;;; Copyright (C) 2017-2022 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:build-generator.commandline-options)

;;; Option conditions

(define-condition option-argument-condition ()
  ((option :initarg :option
           :reader  option
           :documentation
           "Stores the description of the option for which the problem
            occurred."))
  (:default-initargs
   :option (missing-required-initarg 'option-argument-condition :option))
  (:documentation
   "Supertype for argument-related condition types."))

(defmethod designators ((thing option-argument-condition))
  (designators (option thing)))

(define-condition option-does-not-accept-argument-error
    (option-argument-condition
     error)
  ()
  (:report
   (lambda (condition stream)
     (let+ (((&accessors-r/o option) condition))
       (format stream "~@<The \"~A\" option does not take an argument.~@:>"
               (first (designators option))))))
  (:documentation
   "Signaled when an option does not accept a supplied argument."))

(define-condition mandatory-argument-not-supplied-error
    (option-argument-condition
     error)
  ((argument-name :initarg :argument-name
                  :reader  argument-name
                  :documentation
                  "Stores the name of the missing argument."))
  (:default-initargs
   :argument-name (missing-required-initarg
                   'mandatory-argument-not-supplied-error :argument-name))
  (:report
   (lambda (condition stream)
     (let+ (((&accessors-r/o option argument-name) condition))
       (format stream "~@<The \"~A\" option requires a ~A argument.~@:>"
               (first (designators option)) argument-name))))
  (:documentation
   "Signaled when an argument required by an option is not supplied."))

;;; Context conditions

(define-condition option-context-condition (condition)
  ((context :initarg :context
            :type    string
            :reader  context
            :documentation
            "Stores the context for which the condition was
             signaled."))
  (:default-initargs
   :context (missing-required-initarg 'option-condition :context))
  (:documentation
   "Superclass for conditions involving an option context."))

(define-condition context-not-found-error (option-context-condition
                                           error)
  ()
  (:documentation
   "Signaled when a requested context does not exist."))

(define-condition option-condition (condition)
  ((option  :initarg :option
            :type    option-designator
            :reader  option
            :documentation
            "Stores a designator of the option for which the condition
             was signaled."))
  (:default-initargs
   :option (missing-required-initarg 'option-condition :option))
  (:documentation
   "Superclass for option-related conditions."))

(define-condition option-not-found-error (option-context-condition
                                          option-condition
                                          error)
  ()
  (:report
   (lambda (condition stream)
     (let+ (((&accessors-r/o (context-name context) option) condition)
            (positional? (integerp option))
            (matches     (unless positional?
                           (when-let* ((context     (find-options context-name
                                                                  :if-does-not-exist nil))
                                       (designators (mappend #'designators context))
                                       (names       (remove-if-not #'stringp designators)))
                             (util:closest-matches option names :limit 3)))))
       (format stream "~@<~:[\"~A\" is not a known option of~;A ~
                       positional option at position ~D is not valid ~
                       for~] the \"~A\" context.~@[ Closest match~
                       ~[~;~:;es~]: ~{~A~^, ~}.~]~@:>"
               positional? option context-name
               (when matches (length matches)) matches))))
  (:documentation
   "Signaled when a specified option does not exist."))

(define-condition option-supplied-multiple-times-error
    (option-context-condition
     option-condition
     error)
  ()
  (:report
   (lambda (condition stream)
     (let+ (((&accessors-r/o context option) condition))
       (format stream "~@<The \"~A\" option has been supplied more ~
                       than once in the \"~A\" context.~@:>"
               option context))))
  (:documentation
   "Signaled when a singleton option is supplied more than once."))

(define-condition mandatory-options-not-supplied-error
    (option-context-condition
     error)
  ((missing :initarg :missing
            :type    list
            :reader  missing
            :documentation
            "Stores the option info objects for the unsupplied
             options."))
  (:default-initargs
   :missing (missing-required-initarg 'mandatory-options-not-supplied-error :missing))
  (:report
   (lambda (condition stream)
     (let+ (((&accessors-r/o context missing) condition))
       (format stream "~@<The following option~*~P ~
                       ~:*~[~;is~:;are~]~2:* mandatory for the \"~A\" ~
                       context but ~[~;has~:;have~] not been supplied: ~
                       ~{~
                         ~/build-generator.commandline-options:print-option/~
                         ~^, ~
                       ~}.~@:>"
               context (length missing) missing))))
  (:documentation
   "Signaled when at least one mandatory option is not supplied."))

(define-condition option-argument-error (option-context-condition
                                         chainable-condition
                                         error)
  ()
  (:report
   (lambda (condition stream)
     (format stream "~A" (cause condition))))
  (:documentation
   "Signaled when something is wrong with the argument to an option."))
