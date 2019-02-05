;;;; command-language-server.lisp --- Command for running a language server.
;;;;
;;;; Copyright (C) 2017, 2018, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.project.commands)

(defclass language-server ()
  ()
  (:documentation
   #.(format nil "Start a language server for use by IDEs.~@
      ~@
      The language server implements version TODO of the language ~
      server protocol and operates in stdin/stdout mode.~@
      ~@
      The language server implements multiple languages to for the ~
      different recipes types:~@
      ~@
      • template-recipe~@
      • project-recipe~@
      • distribution-recipe~@
      ~@
      For now, a separate workspace is created for each recipe type, ~
      the respective root directory being the corresponding ~
      sub-directory of the recipe repository: projects/ for the ~
      project-recipe language and similarly for the others.")))

(service-provider:register-provider/class
 'command :language-server :class 'language-server)

(jenkins.project.commandline-options:define-option-mapping
    (*command-schema* "language-server"))

(defmethod command-execute ((command language-server))
  (log4cl:remove-all-appenders log4cl:*root-logger*)
  (log:config :info :daily "/tmp/build-generator-language-server.log")

  ;; Swank. TODO move this somewhere else
  (ignore-errors (delete-file "/tmp/port.txt"))
  (uiop:symbol-call '#:swank '#:start-server "/tmp/port.txt" :dont-close t)

  ;; TODO (lsp:language-server *standard-input* *standard-output*)
  (let* ((connection (setf jenkins.language-server::*connection*
                           (protocol.language-server.connection:make-connection
                            *standard-input* *standard-output*)))

         (context    (protocol.language-server::make-context
                      connection :workspace-class 'jenkins.language-server::workspace)))
    (handler-bind ((error (lambda (condition)
                            (format *error-output* "Unhandled error processing request~%")
                            (ignore-errors (format *error-output* "~A" condition))
                            (ignore-errors (sb-debug:print-backtrace :stream *error-output*))
                            (continue))))
      (protocol.language-server::process-requests connection context))))
