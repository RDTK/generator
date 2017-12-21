;;;; command-install.lisp --- Install a Jenkins instance.
;;;;
;;;; Copyright (C) 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.project.commands)

(defparameter *default-plugins*
  '("git" "mercurial" "build-flow-plugin" "build-timeout" "redmine"
    "warnings" "copyartifact" "publish-over-ssh" "sloccount" "tasks"
    "warnings" "xunit" "junit" "blame-upstream-committers" "email-ext"))

(defclass install (output-directory-mixin)
  ((output-directory      :documentation
                          #.(format nil "Destination directory for the ~
                             Jenkins installation."))
   (overwrite?            :initarg  :overwrite?
                          :type     boolean
                          :reader   overwrite?
                          :initform nil
                          :documentation
                          #.(format nil "Overwrite an existing ~
                             installation if there is one?"))
   ;; Jenkins download
   (jenkins-download-url  :initarg  :jenkins-download-url
                          :type     puri:uri
                          :reader   jenkins-download-url
                          ; :initform project-automation.steps::+default-jenkins-download-url+
                          :documentation
                          "TODO")
   (plugins               :initarg  :plugins
                          :type     (or null (cons string list))
                          :reader   plugins
                          :initform *default-plugins*
                          :documentation
                          "List of plugins to install.")
   ;; User creation
   (username              :initarg  :username
                          :type     string
                          :reader   username
                          :documentation
                          "Username for the initial user account.")
   (email                 :initarg  :email
                          :type     string
                          :reader   email
                          :documentation
                          "Email-address for the initial user account.")
   (password              :initarg  :password
                          :type     string
                          :reader   password
                          :documentation
                          "Password for the initial user account."))
  (:default-initargs
   :username (missing-required-initarg 'install :username)
   :email    (missing-required-initarg 'install :email)
   :password (missing-required-initarg 'install :password))
  (:documentation
   "Install and configure a local Jenkins CI server.

    Download Jenkins itself
    Download and install Jenkins plugins needed by the build generator
    Set up an user account"))

(service-provider:register-provider/class
 'command :install :class 'install)

(jenkins.project.commandline-options:define-option-mapping
    (*command-schema* "install")
  (0                    "output-directory" "DIRECTORY"     t)

  (("--overwrite" "-o") "overwrite?")

  (("--username" "-u")  "username"         "USENAME"       t)
  (("--email" "-e")     "email"            "EMAIL-ADDRESS" t)
  (("--password" "-p")  "password"         "PASSWORD"      t))

(defmethod command-execute ((command install))
  (let+ (((&accessors-r/o output-directory overwrite?
                          jenkins-download-url plugins
                          username email password)
          command))

    ))
