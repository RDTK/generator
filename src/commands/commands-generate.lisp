;;;; commands-generate.lisp --- Generate different kinds of output based on a distribution.
;;;;
;;;; Copyright (C) 2018-2022 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:build-generator.commands)

;;; `generate-target'
;;;
;;; A command that delegates most of the actual work to a provider of
;;; the `deploy:target' service. Multiple providers of the `command'
;;; service (one for each provider of the `deploy:target' service) are
;;; registered using this class and `target-provider' (defined below).

(defclass generate-target (distribution-input-mixin
                           mode-mixin)
  ((%target :initarg  :target
            :reader   target))
  (:default-initargs
   :target (missing-required-initarg 'generate-target :target))
  (:documentation
   "Generate non-Jenkins things for a given distribution."))

(defmethod command-execute ((command generate-target))
  (let+ (((&accessors-r/o distributions mode overwrites target)
          command)
         ((&values distributions projects)
          (generate-load distributions mode overwrites
                         :generator-version (generator-version)
                         :cache-directory   *cache-directory*))
         (distributions
          (generate-analyze distributions projects
                            :generator-version (generator-version)
                            :cache-directory   *cache-directory*
                            :temp-directory    *temp-directory*))
         (distributions
          (as-phase (:instantiate)
            (mapcan (lambda (distribution-spec)
                      (when-let ((distribution (model:instantiate
                                                distribution-spec)))
                        (list distribution)))
                    distributions))))
    (as-phase (:check-access ; :continuable? nil
               )
      (check-distribution-access distributions))

    (as-phase (:generate)
      (deploy:deploy distributions target))

    (as-phase (:display-messages)
      (map nil #'display-messages distributions))))

(defun display-messages (distribution &key (stream *standard-output*))
  (let+ ((first? t)
         ((&labels display-message (object message)
            (if first?
                (setf first? nil)
                (format stream "~2%"))
            (format stream "Message for ~/print-items:format-print-items/~@
                            ~@
                            ~2@T~@<~@;~A~:>"
                    (print-items:print-items object) message)))
         ((&labels maybe-display-message (object variable)
            (when-let ((message (var:value object variable nil)))
              (display-message object message)))))
    (maybe-display-message distribution :message)
    (map nil (rcurry #'maybe-display-message :message)
         (project:versions distribution))
    (unless first?
      (fresh-line stream))))

;;; `target-provider'
;;;
;;; A proxy provider that forwards most requests to a given
;;; implementation of the `deploy:target' service.
;;;
;;; The `make-provider' method, however, not only instantiates the
;;; given provider of the `deploy:target' service but also wraps the
;;; result in a `generate-target' instance. The result is thus an
;;; implementation of the command service.

(defclass target-provider (print-items:print-items-mixin)
  ((%name   :initarg :name
            :reader  service-provider:provider-name)
   (%target :initarg :target
            :reader  target)))

(flet ((find-target-provider (target-provider)
         (let ((target-service (service-provider:find-service 'deploy:target)))
           (values (service-provider:find-provider
                    target-service (target target-provider))
                   target-service))))

  (defmethod print-items:print-items append ((object target-provider))
    (let ((name   (service-provider:provider-name object))
          (target (print-items:print-items (find-target-provider object))))
      `((:name                    "~A"                                                ,name)
        ((:target (:after :name)) "~@[ ~/utilities.print-items:format-print-items/~]" ,target))))

  (defmethod documentation ((object target-provider) (type (eql t)))
    (documentation (find-target-provider object) t))

  (defmethod service-provider:provider-class ((provider target-provider))
    (service-provider:provider-class (find-target-provider provider)))

  (defmethod configuration.options.service-provider:provider-schema
      ((service t) (provider target-provider) &key documentation)
    (let+ (((&values provider target-service) (find-target-provider provider))
           (schema   (configuration.options.service-provider:provider-schema
                      target-service provider :documentation documentation)))
      (loop :for (kind name item) :in (configuration.options.mop:class-schema-items
                                       (find-class 'generate-target))
            :unless (string= name "%target")
            :do (ecase kind
                  (:item
                   (setf (configuration.options:find-option name schema) item))))
      schema)))

(defmethod service-provider:make-provider
    ((service t) (provider target-provider)
     &rest initargs &key distributions
                         mode
                         overwrites)
  (let ((target (apply #'deploy:make-target (target provider)
                       (remove-from-plist initargs :distributions :mode :overwrites))))
    (make-instance 'generate-target :distributions distributions
                                    :mode          (or mode "toolkit") ; TODO
                                    :overwrites    overwrites
                                    :target        target)))

;;; Target-based generate command providers

(defmacro define-generate-command ((command-name target-name)
                                   &body option-mapping-clauses)
  (let ((context-name (string-downcase command-name)))
    `(progn
       (service-provider:register-provider
        'command ,command-name 'target-provider :target ,target-name)

       (build-generator.commandline-options:define-option-mapping
           (*command-schema* ,context-name)
         (&rest           "distributions" "DISTRIBUTION-NAME"   t)

         (("--mode" "-m") "mode"          "MODE")
         (("--set" "-D")  "overwrites"    "VARIABLE-NAME=VALUE")

         ,@option-mapping-clauses))))

;;; Command for Jenkins target

(progn
  (define-generate-command (:generate-jenkins :jenkins)
    . #.(append
         #1='(("--delete-other"         "delete-other?")
              ("--delete-other-pattern" "delete-other-pattern" "REGEX")

              (("--base-uri" "-b")      "base-uri"             "URI")
              (("--username" "-u")      "username"             "LOGIN")
              (("--password" "-p")      "password"             "PASSWORD"))
         '((("--api-token" "-t")     "api-token"            "API-TOKEN"))))

  (define-generate-command (:generate :jenkins) ; Backward compatibility
    . #.(append
         #1#
         '((("--api-token" "-t" "-a") "api-token" "API-TOKEN")))))

;;; Command for Dockerfile target

(define-generate-command (:generate-dockerfile :dockerfile)
  (("--output-directory" "-o") "output-directory" "DIRECTORY"             t)

  (("--base-image" "-b")       "base-image"       "IMAGE-NAME[:TAG-NAME]" t)
  (("--staging-image" "-s")    "staging-image"    "IMAGE-NAME[:TAG-NAME]")
  (("--platform" "-p")         "platform"         "PLATFORM")

  (("--run-strategy" "-r")     "run-strategy"     "STRATEGY"))

;;; Command for Makefile target

(define-generate-command (:generate-makefile :makefile)
  (("--output-directory" "-o") "output-directory" "DIRECTORY" t))

;;; Command for build target

(define-generate-command (:build :build)
  (("--working-directory" "-w") "working-directory"         "DIRECTORY" t)
  ("--delete-working-directory" "delete-working-directory?" "BOOLEAN"))
