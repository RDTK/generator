;;;; command-install-jenkins.lisp --- Install a Jenkins instance.
;;;;
;;;; Copyright (C) 2017, 2018, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.project.commands)

;;; Required and additional plugins

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; TODO should be defined in aspects module
  (defmethod project:requires ((thing aspects::aspect))
    (map 'list (curry #'list :jenkins-plugin)
         (aspects::required-plugins thing)))

  (defun required-jenkins-plugins ()
    (let ((aspect-classes (map 'list #'service-provider:provider-class
                               (service-provider:service-providers
                                'aspects::aspect))))
      (remove-duplicates
       (mappend (lambda (aspect-class)
                  (let* ((prototype    (c2mop:class-prototype
                                        (c2mop:ensure-finalized aspect-class)))
                         (requirements (project:requires-of-kind
                                        :jenkins-plugin prototype)))
                    (map 'list #'second requirements)))
                aspect-classes)
       :test #'string=))))

(defparameter *default-extra-plugins*
  '("extra-columns"))

;;; Jenkins installation profiles

(defclass profile ()
  ((%name          :initarg :name
                   :type    keyword
                   :reader  name)
   (%extra-plugins :initarg :extra-plugins
                   :type    list
                   :reader  extra-plugins)))

(defparameter *profiles*
  (flet ((profile (name &optional extra-plugins)
           `(,name . ,(make-instance 'profile
                                     :name          name
                                     :extra-plugins extra-plugins))))
    `(,(profile :single-user)
      ,(profile :local-docker '("docker-plugin")))))

(defun find-profile (name)
  (assoc-value *profiles* name))

;;; Command

(defclass install-jenkins (output-directory-mixin)
  (;; Output
   (output-directory     :documentation
                         #.(format nil "Destination directory for the ~
                            Jenkins installation."))
   ;; Profile
   (profile              :initarg  :profile
                         :type     (member :single-user :local-docker)
                         :reader   profile
                         :initform :single-user
                         :documentation
                         #.(format nil "A Jenkins usage profile to ~
                            which the installation should be ~
                            tailored."))
   ;; Jenkins download
   (jenkins-download-url :initarg  :jenkins-download-url
                         :type     puri:uri
                         :reader   jenkins-download-url
                         :initform steps::+default-jenkins-download-url+
                         :documentation
                         #.(format nil "URL from which the Jenkins ~
                            archive should be downloaded."))
   (plugins              :initarg  :plugins
                         :type     (or null (cons string list))
                         :reader   plugins
                         :initform *default-extra-plugins*
                         :documentation
                         #.(format nil "List of plugins to install in ~
                            addition to the required ones.~@
                            ~@
                            The following plugins are required and ~
                            will be installed in any case:~%~{• ~
                            ~A~^~%~}"
                                   (sort (copy-list (required-jenkins-plugins))
                                         #'string<)))
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
      • Optionally set up a user account.")))

(service-provider:register-provider/class
 'command :install-jenkins :class 'install-jenkins)

(jenkins.project.commandline-options:define-option-mapping
    (*command-schema* "install-jenkins")
  (0                        "output-directory"     "DIRECTORY"     t)

  ("--profile"              "profile"              "PROFILE")

  ("--jenkins-download-url" "jenkins-download-url" "URL")

  (("--plugin" "-p")        "plugins"              "PLUGIN")

  (("--username" "-u")      "username"             "USENAME")
  (("--email" "-e")         "email"                "EMAIL-ADDRESS")
  (("--password" "-p")      "password"             "PASSWORD"))

(defmethod command-execute ((command install-jenkins))
  (let+ (((&accessors-r/o output-directory
                          (profile-name profile)
                          jenkins-download-url plugins
                          username email password)
          command)
         (profile     (find-profile profile-name))
         (all-plugins (remove-duplicates
                       (append (required-jenkins-plugins)
                               (extra-plugins profile)
                               plugins)
                       :test #'string=)))
    (as-phase (:install)
      (with-trivial-progress (:install/core)
        (steps:execute (steps:make-step :jenkins/install-core) nil
                       :destination-directory output-directory
                       :url                   jenkins-download-url))

      (steps:execute
       (steps:make-step :jenkins/install-plugins-with-dependencies) nil
       :destination-directory output-directory
       :plugins               all-plugins))

    (as-phase (:configure)
      (steps:execute (steps:make-step :jenkins/install-config-files) nil
                     :destination-directory output-directory
                     :profile               (name profile))

      (when (and username email password)
        (steps:execute (steps:make-step :jenkins/create-user) nil
                       :destination-directory output-directory
                       :username              username
                       :email                 email
                       :password              password)))))
