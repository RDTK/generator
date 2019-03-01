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
                         "List of plugins to install."))
  (:documentation
   #.(format nil "Install and configure a Jenkins CI server.~@
      ~@
      • Download Jenkins itself.~@
      ~@
      • Download and install Jenkins plugins needed by the build ~
        generator.~@
      ~@
      • Install Jenkins configuration files that make the instance ~
        work well with the build-generator.")))

(service-provider:register-provider/class
 'command :install-jenkins :class 'install-jenkins)

(jenkins.project.commandline-options:define-option-mapping
    (*command-schema* "install-jenkins")
  (0                        "output-directory"     "DIRECTORY" t)

  ("--jenkins-download-url" "jenkins-download-url" "URL")

  (("--plugin" "-p")        "plugins"              "PLUGIN"))

(defmethod command-execute ((command install-jenkins))
  (let+ (((&accessors-r/o output-directory
                          jenkins-download-url plugins)
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
       :config-files          jenkins.project.steps::*jenkins-config-files*))))
