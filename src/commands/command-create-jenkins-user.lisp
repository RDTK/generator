;;;; command-create-jenkins-user.lisp --- Create a user in a Jenkins instance.
;;;;
;;;; Copyright (C) 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.project.commands)

(defclass create-jenkins-user (output-directory-mixin)
  (;; Output
   (output-directory :documentation
                     #.(format nil "Home directory of the Jenkins ~
                        instance in which the user should be ~
                        created."))
   ;; User creation
   (username         :initarg  :username
                     :type     (or null string)
                     :reader   username
                     :initform nil
                     :documentation
                     "Username for the new user account.")
   (email            :initarg  :email
                     :type     (or null string)
                     :reader   email
                     :initform nil
                     :documentation
                     "Email-address for the new user account.")
   (password         :initarg  :password
                     :type     (or null string)
                     :reader   password
                     :initform nil
                     :documentation
                     "Password for the new user account."))
  (:documentation
   #.(format nil "Create a user account in a Jenkins instance.")))

(service-provider:register-provider/class
 'command :create-jenkins-user :class 'create-jenkins-user)

(jenkins.project.commandline-options:define-option-mapping
    (*command-schema* "create-jenkins-user")
  (0                   "output-directory" "DIRECTORY"     t)

  (("--username" "-u") "username"         "USENAME"       t)
  (("--email" "-e")    "email"            "EMAIL-ADDRESS" t)
  (("--password" "-p") "password"         "PASSWORD"      t))

(defmethod command-execute ((command create-jenkins-user))
  (let+ (((&accessors-r/o output-directory username email password) command))
    (as-phase (:configure)
      (jenkins.project.steps:execute
       (jenkins.project.steps:make-step :jenkins/create-user) nil
       :destination-directory output-directory
       :username              username
       :email                 email
       :password              password))))
