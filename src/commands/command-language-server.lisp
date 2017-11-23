;;;; command-language-server.lisp --- Command for starting a language server.
;;;;
;;;; Copyright (C) 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.project.commands)

#.(progn
    #1=(asdf:load-system :swank)
    '#1#)

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

  (jenkins.language-server:language-server *standard-input* *standard-output*))
