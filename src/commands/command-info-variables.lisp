;;;; command-info-variables.lisp --- Command for printing variable information.
;;;;
;;;; Copyright (C) 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.project.commands)

(defclass info-variables ()
  ()
  (:documentation
   "Print information about recognized variables."))

(service-provider:register-provider/class
 'command :info-variables :class 'info-variables)

(jenkins.project.commandline-options:define-option-mapping
    (*command-schema* "info-variables"))

(defmethod command-execute ((command info-variables))
  (let ((stream *standard-output*)
        (sorted (sort (copy-list (all-variables)) #'string<
                      :key #'variable-info-name)))
    (format stream "~@<~{~{~
                      \"~(~A~)\"~@[: ~(~A~)~]~
                      ~@[~@:_~2@T~<~A~:>~]~
                    ~}~^~@:_~@:_~}~:>"
            (mapcar (lambda (variable)
                      (let+ (((&structure-r/o variable-info- name type documentation)
                              variable))
                        (list name
                              (unless (eq type t) type)
                              (when documentation (list documentation)))))
                    sorted))))
