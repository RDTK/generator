;;;; command-install-jenkins.lisp --- Install a Jenkins instance.
;;;;
;;;; Copyright (C) 2017, 2018, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.project.commands)

(defparameter *default-plugins*
  '("git" "subversion" "mercurial"

    "redmine" "github"

    "build-timeout"

    "groovy" "copyartifact"

    "publish-over-ssh" "sloccount" "tasks"
    "warnings" "xunit" "htmlpublisher"

    "extra-columns"))

(defclass install-jenkins (output-directory-mixin)
  (;; Output
   (output-directory     :documentation
                         #.(format nil "Destination directory for the ~
                            Jenkins installation."))
   ;; Jenkins download
   (jenkins-download-url :initarg  :jenkins-download-url
                         :type     puri:uri
                         :reader   jenkins-download-url
                         :initform jenkins.project.steps::+default-jenkins-download-url+
                         :documentation
                         #.(format nil "URL from which the Jenkins ~
                            archive should be downloaded."))
   (plugins              :initarg  :plugins
                         :type     (or null (cons string list))
                         :reader   plugins
                         :initform *default-plugins*
                         :documentation
                         "List of plugins to install.")
   ;; User creation
   (username             :initarg  :username
                         :type     (or null string)
                         :reader   username
                         :initform nil
                         :documentation
                         "Username for the initial user account.")
   (email                :initarg  :email
                         :type     (or null string)
                         :reader   email
                         :initform nil
                         :documentation
                         "Email-address for the initial user account.")
   (password             :initarg  :password
                         :type     (or null string)
                         :reader   password
                         :initform nil
                         :documentation
                         "Password for the initial user account."))
  (:documentation
   #.(format nil "Install and configure a Jenkins CI server.~@
      ~@
      • Download Jenkins itself.~@
      ~@
      • Download and install Jenkins plugins needed by the build ~
        generator.~@
      ~@
      • Install Jenkins configuration files that make the instance ~
        work well with the build-generator.~@
      ~@
      • Set up a user account.")))

(service-provider:register-provider/class
 'command :install-jenkins :class 'install-jenkins)

(jenkins.project.commandline-options:define-option-mapping
    (*command-schema* "install-jenkins")
  (0                        "output-directory"     "DIRECTORY"     t)

  ("--jenkins-download-url" "jenkins-download-url" "URL")

  (("--plugin" "-p")        "plugins"              "PLUGIN")

  (("--username" "-u")      "username"             "USENAME")
  (("--email" "-e")         "email"                "EMAIL-ADDRESS")
  (("--password" "-p")      "password"             "PASSWORD"))

(defmethod command-execute ((command install-jenkins))
  (let+ (((&accessors-r/o output-directory
                          jenkins-download-url plugins
                          username email password)
          command))
    (as-phase (:install)
      (with-trivial-progress (:install/core)
        (jenkins.project.steps:execute
         (jenkins.project.steps:make-step :jenkins/install-core) nil
         :destination-directory output-directory
         :url                   jenkins-download-url))

      (jenkins.project.steps:execute
       (jenkins.project.steps:make-step :jenkins/install-plugins-with-dependencies) nil
       :destination-directory output-directory
       :plugins               plugins))

    (as-phase (:configure)
      (jenkins.project.steps:execute
       (jenkins.project.steps:make-step :jenkins/install-config-files) nil
       :destination-directory output-directory
       :config-files          jenkins.project.steps::*jenkins-config-files*)

      (when (and username email password)
        (jenkins.project.steps:execute
         (jenkins.project.steps:make-step :jenkins/create-user) nil
         :destination-directory output-directory
         :config-file-template  jenkins.project.steps::*jenkins-user-config-file-template*
         :username              username
         :email                 email
         :password              password)))))
