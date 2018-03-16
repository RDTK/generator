;;;; command-info-variables.lisp --- Command for printing variable information.
;;;;
;;;; Copyright (C) 2017, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.project.commands)

(defclass info-variables ()
  ((filter :type     (or null string function) ; TODO string is a hack
           :reader   filter
           :accessor %filter
           :initform nil
           :documentation
           "Restrict output to matching variables."))
  (:documentation
   "Print information about recognized variables."))

(defmethod shared-initialize :after ((instance info-variables) (slot-names t)
                                     &key
                                     (filter nil filter-supplied?))
  (when filter-supplied?
    (setf (%filter instance)
          (etypecase filter
            ((or null function)
             filter)
            (string
             (lambda (variable-info)
               (let ((name (string-downcase
                            (variable-info-name variable-info))))
                 (ppcre:scan filter name))))))))

(service-provider:register-provider/class
 'command :info-variables :class 'info-variables)

(jenkins.project.commandline-options:define-option-mapping
    (*command-schema* "info-variables")
  (("--filter" "-f") "filter" "REGEX"))

(defmethod command-execute ((command info-variables))
  (let* ((stream   *standard-output*)
         (relevant (if-let ((filter (filter command)))
                     (delete-if-not filter (copy-list (all-variables)))
                     (copy-list (all-variables))))
         (sorted   (sort relevant #'string< :key #'variable-info-name)))
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
