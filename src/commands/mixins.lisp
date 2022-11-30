;;;; mixins.lisp --- Mixins classes used in the commands module.
;;;;
;;;; Copyright (C) 2017-2022 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:build-generator.commands)

;;; `distribution-input-mixin'

(defclass distribution-input-mixin ()
  ((distributions :initarg  :distributions
                  :type     (or null (cons string list))
                  :reader   distributions
                  :documentation
                  "Distribution recipes(s) which should be processed."))
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
               #.(format nil "The mode according to which build ~
                  commands should be selected.~@
                  ~@
                  Selects the set of templates stored in the ~
                  templates/MODE directory."))
   (overwrites :initarg  :overwrites
               :type     (or null (cons variable-assignment list))
               :reader   overwrites
               :initform '()
               :documentation
               #.(format nil "Overwrite a variable after loading the ~
                  distribution(s).~@
                  ~@
                  Arguments to this option have to be of the form ~
                  VARIABLE-NAME=VALUE.")))
  (:documentation
   "Adds a mode and an overwrites slot to command classes."))

;;; `output-directory-mixin'

(defclass output-directory-mixin ()
  ((output-directory :initarg  :output-directory
                     :type     configuration.options:directory-pathname
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
  ((base-uri  :initarg  :base-uri
              :type     puri:uri
              :reader   base-uri
              :initform (puri:uri "https://localhost:8080")
              :documentation
              "Jenkins base URI.")
   (username  :initarg  :username
              :type     (or null string)
              :reader   username
              :initform nil
              :documentation
              "Username for Jenkins authentication.")
   (password  :initarg  :password
              :type     (or null string)
              :reader   password
              :initform nil
              :documentation
              "Password for Jenkins authentication.")
   (api-token :initarg  :api-token
              :type     (or null string)
              :reader   api-token
              :initform nil
              :documentation
              "API token for Jenkins authentication."))
  (:documentation
   "Adds infrastructure for accessing a Jenkins server to command classes."))

(defmethod command-execute :around ((command jenkins-access-mixin))
  (let+ (((&accessors-r/o base-uri username password api-token) command)
         (credentials (cond (api-token
                             (jenkins.api:make-token-credentials
                              username api-token))
                            ((and username password)
                             (jenkins.api:make-username+password-credentials
                              username password))))
         (endpoint    (jenkins.api:make-endpoint
                       base-uri :credentials credentials)))
    (jenkins.api:with-endpoint (endpoint)
      (call-next-method))))

(defmethod command-execute :before ((command jenkins-access-mixin))
  (as-phase (:verify-jenkins) (jenkins.api::verify-jenkins)))
