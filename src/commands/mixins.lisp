;;;; mixins.lisp --- Mixins classes used in the commands module.
;;;;
;;;; Copyright (C) 2017, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.project.commands)

;;; `distribution-input-mixin'

(defclass distribution-input-mixin ()
  ((distributions :initarg  :distributions
                  :type     (or null (cons string list))
                  :reader   distributions
                  :documentation
                  "Distribution(s) for which jobs should be generated."))
  (:default-initargs
   :distributions (missing-required-initarg
                   'distribution-input-mixin :distributions))
  (:documentation
   "Adds distributions slot to command classes."))

;;; `mode-mixin'

(defclass mode-mixin ()
  ((mode       :initarg  :mode
               :type     string
               :reader   mode
               :initform "toolkit"
               :documentation
               #.(format nil "The mode according to which jobs should ~
                  be generated.~@
                  ~@
                  Selects the set of templates stored in the ~
                  templates/MODE directory."))
   (overwrites :initarg  :overwrites
               :type     (or null (cons variable-assignment list))
               :reader   overwrites
               :initform '()
               :documentation
               #.(format nil "Overwrite a variable after loading the ~
                  distribution.~@
                  ~@
                  Arguments to this option have to be of the form ~
                  VARIABLE-NAME=VALUE. This option can be supplied ~
                  multiple times.")))
  (:documentation
   "Adds a mode and an overwrites slot to command classes."))

;;; `output-directory-mixin'

(defclass output-directory-mixin ()
  ((output-directory :initarg  :output-directory
                     :type     pathname
                     :reader   output-directory
                     :documentation
                     "Directory into which output should be written."))
  (:default-initargs
   :output-directory (missing-required-initarg
                      'output-directory-mixin :output-directory))
  (:documentation
   "Adds an output-directory slot to command classes."))

;;; `jenkins-access-mixin'

(defclass jenkins-access-mixin ()
  ((base-uri :initarg  :base-uri
             :type     puri:uri
             :reader   base-uri
             :initform (puri:uri "https://localhost:8080")
             :documentation
             "Jenkins base URI.")
   (username :initarg  :username
             :type     (or null string)
             :reader   username
             :initform nil
             :documentation
             "Username for Jenkins authentication.")
   (password :initarg  :password
             :type     (or null string)
             :reader   password
             :initform nil
             :documentation
             "Password for Jenkins authentication."))
  (:documentation
   "Adds infrastructure for accessing a Jenkins server to command classes."))

(defmethod command-execute :around ((command jenkins-access-mixin))
  (let+ (((&accessors-r/o (jenkins.api:*base-url* base-uri)
                          (jenkins.api:*username* username)
                          (jenkins.api:*password* password))
          command))
    (call-next-method)))

(defmethod command-execute :before ((command jenkins-access-mixin))
  (jenkins.api::verify-jenkins))
