;;;; command-info-aspects.lisp --- Command for printing aspect information.
;;;;
;;;; Copyright (C) 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.project.commands)

(defclass info-aspects ()
  ()
  (:documentation
   "Print information about available aspects."))

(service-provider:register-provider/class
 'command :info-aspects :class 'info-aspects)

(jenkins.project.commandline-options:define-option-mapping
    (*command-schema* "info-aspects"))

(defmethod command-execute ((command info-aspects))
  (let* ((stream    *standard-output*)
         (providers (service-provider:service-providers 'jenkins.model.aspects::aspect))
         (providers (sort (copy-list providers) #'string<
                          :key (compose #'string #'service-provider:provider-name))))
    (format stream "~{~<~
                      ~(~A~)~
                      ~@[~@:_~4@T~<~{~{~
                        \"~(~A~)\"~@[: ~(~A~)~]~@[ = ~A~]~
                        ~@[~@:_~A~]~
                      ~}~^~@:_~}~:>~]~
                      ~@[~@:_~2@T~A~]~
                    ~:>~^~2%~}"
            (mapcar (lambda (provider)
                      (list (service-provider:provider-name provider)
                            (when-let ((stuff (mapcar (lambda (parameter)
                                                        (let ((variable (jenkins.model.aspects:aspect-parameter-variable parameter)))
                                                          (list (variable-info-name variable)
                                                                (unless (eq (variable-info-type variable) t)
                                                                  (variable-info-type variable))
                                                                (json:encode-json-to-string
                                                                 (jenkins.model.aspects:aspect-parameter-default-value parameter))
                                                                (variable-info-documentation variable))))
                                                      (jenkins.model.aspects:aspect-parameters
                                                       (service-provider:provider-class provider)))))
                              (list stuff))
                            (documentation provider t)))
                    providers))))
