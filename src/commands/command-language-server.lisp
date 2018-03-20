;;;; command-language-server.lisp --- Command for starting a language server.
;;;;
;;;; Copyright (C) 2017, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.project.commands)

(defclass language-server ()
  ()
  (:documentation
   "Start a language server for use by IDEs."))

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
  (catch 'exit
    (with-output-to-file (*trace-output* "/tmp/trace" :if-exists :supersede)
      (loop :with connection = (setf jenkins.language-server::*connection*
                                     (protocol.language-server.connection:make-connection
                                      *standard-input* *standard-output*))
            :with context = (make-instance 'protocol.language-server::context
                                           :connection      connection
                                           :workspace-class 'jenkins.language-server::workspace)
            :do (protocol.language-server::process-request connection context)))))
