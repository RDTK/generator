;;;; conditions.lisp --- Conditions signaled by the commands module.
;;;;
;;;; Copyright (C) 2017, 2018, 2019 Jan Moringen
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
     (let* ((command  (command condition))
            (commands (service-provider:service-providers/alist
                       (service-provider:find-service 'command)))
            (names    (map 'list (compose #'string-downcase #'car) commands))
            (matches  (jenkins.util:closest-matches
                       command names :limit 3)))
       (format stream "~@<\"~A\" is not a known command.~@[ Closest ~
                      match~[~;~:;es~]: ~{~A~^, ~}.~]~@:>"
               command (when matches (length matches)) matches))))
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

(defun unfulfilled-project-dependency-error? (condition)
  (when (typep condition 'instantiation-error)
    (let ((root-cause (root-cause condition)))
      (typep root-cause 'jenkins.analysis:unfulfilled-project-dependency-error))))

(deftype unfulfilled-project-dependency-error ()
  '(satisfies unfulfilled-project-dependency-error?))

(defun print-unfulfilled-dependencies (stream conditions &optional colon? at?)
  (declare (ignore colon? at?))
  (let ((table (make-hash-table :test #'equal)))
    (loop :for condition :in conditions
          :for specification = (instantiation-condition-specification
                                condition)
          :for cause = (root-cause condition)
          :for dependency = (jenkins.analysis:dependency-condition-dependency
                             cause)
          :for key = (subseq dependency 0 2)
          :do (push (list (print-items:print-items specification)
                          (third dependency))
                    (gethash key table)))
    (format stream "~@<~{~{~
                      No provider for ~{~A~^ ~}~@:_~
                      ~2@T~@<~
                        ~@{~{~
                          ~/print-items:format-print-items/ ~
                          requires ~:[it~;~:*version ~A~]~
                        ~}~^~@:_~}~
                      ~:>~
                    ~}~^~@:_~@:_~}~:>"
            (hash-table-alist table))))

(defun condition-category (condition)
  (cond ((jenkins.util:some-cause
          (of-type 'jenkins.analysis:repository-access-error)
          condition)
         'jenkins.analysis:repository-access-error)
        ((jenkins.util:some-cause
          (of-type 'jenkins.analysis:repository-analysis-error)
          condition)
         'jenkins.analysis:repository-analysis-error)
        (t
         (type-of condition))))

(define-condition deferred-phase-error (phase-error
                                        deferred-problem-condition)
  ()
  (:report
   (lambda (condition stream)
     (let+ (((&accessors-r/o phase conditions) condition)
            (dependency-conditions (remove-if-not #'unfulfilled-project-dependency-error? conditions))
            (other-conditions      (set-difference conditions dependency-conditions )))
       (format stream "~@<~D problem~:P during ~A phase:~@:_~@:_~
                       ~2@T~@<~
                         ~@[~
                           ~/jenkins.project.commands::print-unfulfilled-dependencies/~
                           ~@:_~@:_~
                         ~]~
                         ~{~
                           ~<~A:~
                             ~@:_~2@T~<~A~:>~
                           ~:>~
                           ~^~@:_~@:_~
                         ~}~
                       ~:>~@:>"
               (length conditions) phase
               dependency-conditions
               (mapcar (lambda (condition)
                         (list (condition-category condition) condition))
                       other-conditions)))))
  (:documentation
   "Signaled when deferred errors have accumulated at the end of a
    phase."))

(defun deferred-phase-cerror (phase conditions)
  "Signal a continuable `deferred-phase-error'."
  (with-simple-restart (continue "~@<Ignore problems in phase ~A and ~
                                  continue.~@:>"
                                 phase)
    (error 'deferred-phase-error :phase phase :conditions conditions)))
