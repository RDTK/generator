;;;; command-config.lisp --- Command for inspecting the configuration.
;;;;
;;;; Copyright (C) 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.project.commands)

(defclass config ()
  ((action :initarg  :action
           :type     (member :get :list :tree)
           :reader   action
           :documentation
           "The configuration action to perform.")
   (option :initarg  :option
           :type     (or null string)
           :reader   option
           :initform nil
           :documentation
           #.(format nil "Name(s) of option(s) to which the action ~
              should be applied.")))
  (:default-initargs
   :action (missing-required-initarg 'config :action))
  (:documentation
   #.(format nil "Describe configuration sources and the current ~
      configuration.~@
      ~@
      Configuration information is merged from multiple ~
      sources (highest priority first):~@
      ~@
      • Options specified on the commandline~@
      ~@
      • Environment variables named BUILD_GENERATOR_option_name~@
      ~@
      • Configuration files named build-generator.conf in the current ~
        directory, ~~/.config/ and /etc/. This can be changed using the ~
        BUILD_GENERATOR_CONFIG_FILES environment variable.~@
      ~@
      Setting the BUILD_GENERATOR_CONFIG_DEBUG environment variable to ~
      an arbitrary value causes debug information regarding ~
      configuration sources and the current configuration to ~
      printed during startup.")))

(service-provider:register-provider/class
 'command :config :class 'config)

(jenkins.project.commandline-options:define-option-mapping
    (*command-schema* "config")
  (0 "action" "ACTION" t)
  (1 "option" "OPTION"))

(defmethod command-execute ((command config))
  (let+ (((&accessors-r/o action option) command)
         (configuration *configuration*)
         (stream        *standard-output*))
    (ecase action
      (:get
       (let ((option (configuration.options:find-option
                      option configuration)))
         (format stream "~A~%" (option-value-string option))))
      (:list
       (let ((lines '()))
         (configuration.options:map-matching-options
          (lambda (option &key prefix container)
            (declare (ignore container))
            (push (make-option-line option prefix) lines))
          (or option "**") configuration)
         (print-option-lines stream lines)))
      (:tree
       (describe configuration stream)
       (terpri stream)))))

;;; Utilities

(defun option-value-string (option)
  (let+ (((&values value value?) (configuration.options:value
                                  option :if-no-value nil)))
    (cond ((not value?)
           "«no value»")
          ((configuration.options:name-matches
            (configuration.options:make-name "**.password")
            (configuration.options:option-name option))
           "«secret»")
          (t
           (let ((schema-item (configuration.options:option-schema-item option)))
             (configuration.options:value->string
              schema-item value))))))

(defun print-option-lines (stream lines)
  (let+ (((&flet name-string (name)
            (with-output-to-string (stream)
              (configuration.options:print-name stream name))))
         (name-width  (reduce #'max lines :key (compose #'length
                                                        #'name-string
                                                        #'first)))
         (value-width (reduce #'max lines :key (compose #'length #'second)))
         (sorted      (sort lines #'configuration.options:name< :key #'first)))
    (loop :for (name value source) :in sorted
          :do (let ((*print-right-margin* nil))
                (format stream "~V/configuration.options:print-name/ ~V:A ~A~%"
                        name-width name value-width value source)))))

(defun make-option-line (option prefix)
  (let* ((name           (configuration.options:option-name option))
         (full-name      (configuration.options:merge-names prefix name))
         (values         (configuration.options:option-values option))
         (value-string   (option-value-string option))
         (source         (getf (rest (find-if #'consp values)) :source))
         (source-string  (if source
                             (princ-to-string source)
                             "«no source»")))
    (list full-name value-string source-string)))
