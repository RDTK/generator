;;;; jenkins-install.lisp --- Steps for setting up a Jenkins instance.
;;;;
;;;; Copyright (C) 2015-2025 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:build-generator.steps)

;;; types

(defun jenkins-username? (thing)
  (and (stringp thing)
       (every (lambda (character)
                (or (alphanumericp character)
                    (char= character #\_)
                    (char= character #\-)))
              thing)))

(deftype jenkins-username ()
  '(satisfies jenkins-username?))

(deftype jenkins-directory-state ()
  "Possible states of a Jenkins installation."
  '(member :not-present :fresh :stopped :running))

;;; Utilities

(define-constant +jenkins-war-filename+
  "jenkins.war"
  :test #'string=)

(define-constant +jenkins-war-manifest-filename+
  "META-INF/MANIFEST.MF"
  :test #'string=)

(defun determine-jenkins-version (directory)
  (let* ((war-name (merge-pathnames +jenkins-war-filename+ directory))
         (content  (zip:with-zipfile (zip war-name)
                     (let ((manifest (zip:get-zipfile-entry
                                      +jenkins-war-manifest-filename+ zip)))
                       (sb-ext:octets-to-string
                        (zip:zipfile-entry-contents manifest))))))
    (multiple-value-bind (success? groups)
        (cl-ppcre:scan-to-strings "Jenkins-Version: ([0-9.]+)" content)
      (unless success?
        (error "~@<Could not determine Jenkins version based on ~A.~@:>"
               war-name))
      (aref groups 0))))

(declaim (ftype (function ((or string pathname))
                          (values jenkins-directory-state &optional))
                jenkins-directory-state))
(defun jenkins-directory-state (directory)
  "Determine and return the state of the Jenkins installation in DIRECTORY."
  (let ((queue?        (probe-file (merge-pathnames "queue.xml" directory)))
        (queue-backup? (probe-file (merge-pathnames "queue.xml.bak" directory))))
    (cond ((not (probe-file directory))
           :not-present)
          ((and (not queue?) (not queue-backup?))
           :fresh)
          (queue?
           :stopped)
          ((and (not queue?) queue-backup?)
           :running)
          (t
           (error "~@<Failed to determine execution state of Jenkins ~
                   directory ~A.~@:>"
                  directory)))))

(defun ensure-jenkins-directory-state (expected directory)
  "Ensure that Jenkins installation in DIRECTORY is in EXPECTED state.

   EXPECTED is either a `jenkins-directory-state' or a list of those.

   An error is signaled if DIRECTORY is in none of the states listed
   in EXPECTED."
  (let ((actual (jenkins-directory-state directory)))
    (cond ((and (consp expected) (member actual expected :test #'eq))
           t)
          ((eq actual expected)
           t)
          (t
           (error "~@<Jenkins installation in ~A appears to be in ~
                   state ~A instead of ~{~A~^ or ~}.~@:>"
                  directory actual (ensure-list expected))))))

;;; `jenkins/install-core' step

(define-step (jenkins/install-core)
    (url destination-directory)
  "Download the Jenkins archive into a specified directory."
  (ensure-jenkins-directory-state :not-present destination-directory)
  (let ((pathname (merge-pathnames "jenkins.war" destination-directory)))
    (ensure-directories-exist pathname)
    (when (not (probe-file pathname))
      (analysis::download-file url pathname))))

;;; `jenkins/determine-version'

(define-step (jenkins/determine-version)
    (destination-directory)
  (let ((version (determine-jenkins-version destination-directory)))
    (log:info "~@<Jenkins version is ~A~@:>" version)
    version))

;;; `jenkins/write-wizard-state'

(define-constant +jenkins-wizard-state-filename+
  "jenkins.install.UpgradeWizard.state"
  :test #'string=)

(define-constant +jenkins-install-util-state-filename+
  "jenkins.install.InstallUtil.lastExecVersion"
  :test #'string=)

(define-step (jenkins/write-wizard-state)
    (destination-directory version)
  (flet ((write-one (filename)
           (let ((filename (merge-pathnames filename destination-directory)))
             (with-output-to-file (stream filename)
               (write-string version stream)))))
    (write-one +jenkins-wizard-state-filename+)
    (write-one +jenkins-install-util-state-filename+)))

;;; `jenkins/get-update-info', `jenkins-download-plugins' and `jenkins/install-plugins' steps

(define-constant +default-jenkins-update-url+
    (puri:uri "https://westeurope.cloudflare.jenkins.io/current/update-center.actual.json")
  :test #'puri:uri=)

(defun download-update-info (&key (url +default-jenkins-update-url+))
  (multiple-value-bind (body code headers origin reply-stream close-stream? reason)
      (let ((drakma:*text-content-types* (list* '("application" . "json")
                                                drakma:*text-content-types*)))
        (drakma:http-request url))
    (declare (ignore origin reply-stream close-stream?))
    (unless (<= 200 code 299)
      (error "~@<Download from ~A failed with code ~D: ~A.~@:>"
             url code reason))
    (let ((content-type (assoc-value headers :content-type)))
      (unless (equal #1="application/json" content-type)
        (error "~@<Expected content-type ~A but got ~A.~@:>"
               #1# content-type)))
    body))

(define-step (jenkins/get-update-info)
    ()
  (with-trivial-progress (:jenkins/get-update-info)
    (let* ((body    (download-update-info)) ; TODO: retry
           (info    (let ((json:*json-identifier-name-to-lisp* #'identity)
                          (json:*identifier-name-to-key*       #'identity))
                      (json:decode-json-from-string body)))
           (core    (assoc-value info "core" :test #'equal))
           (version (assoc-value core "version" :test #'equal))
           (url     (assoc-value core "url" :test #'equal)))
      (values info version url))))

(defun collect-download-urls (update-info plugin-names)
  (let ((plugins        (assoc-value update-info "plugins" :test #'equal))
        (seen           (make-hash-table :test #'equal))
        (names-and-urls '()))
    (labels ((find-plugin (name)
               (assoc name plugins :test #'equal))
             (visit-dependency (dependency reason)
               (let ((name      (assoc-value dependency "name"     :test #'equal))
                     (optional? (assoc-value dependency "optional" :test #'equal))
                     (version   (assoc-value dependency "version"  :test #'equal)))
                 (unless optional?
                   (let ((reason (format nil "required by ~A" reason)))
                     (consider name version reason)))))
             (visit (name info &optional required-version)
               (declare (ignore required-version))
               (let ((version      (assoc-value info "version"      :test #'equal))
                     (url          (assoc-value info "url"          :test #'equal))
                     (dependencies (assoc-value info "dependencies" :test #'equal)))
                 (declare (ignore version))
                 (pushnew (cons name url) names-and-urls :test #'equal)
                 (mapc (rcurry #'visit-dependency name) dependencies)))
             (consider (name &optional version reason)
               (unless (gethash name seen)
                 (setf (gethash name seen) t)
                 (if-let ((cell (find-plugin name)))
                   (destructuring-bind (name . info) cell
                     (visit name info version))
                   (cerror "Do not install the plugin and continue"
                           "~@<Could not find information for plugin `~A'~
                            ~@[ version ~A~]~@[ ~A~].~@:>"
                           name version reason)))))
      (mapc #'consider plugin-names))
    names-and-urls))

(define-constant +jenkins-plugin-directory+
  #P"plugins/"
  :test #'equalp)

(define-constant +jenkins-plugins-base-url+
    (puri:uri "https://updates.jenkins-ci.org/latest/")
  :test #'puri:uri=)

(defun jenkins-plugin-pathname (base-directory name)
  (merge-pathnames (make-pathname :name     name
                                  :type     "jpi"
                                  :defaults +jenkins-plugin-directory+)
                   base-directory))

(define-sequence-step (jenkins/download-plugins name-and-url plugins
                                                :execution :parallel)
    (destination-directory)
  "Add specified plugins to an existing Jenkins installation."
  (destructuring-bind (name . url) name-and-url
    (progress "~A" name)
    (let ((pathname (jenkins-plugin-pathname destination-directory name)))
      (unless (probe-file pathname)
        (log:info "~@<Installing `~A' from ~A into ~A.~@:>" name url pathname)
        (analysis::download-file url pathname)))))

(define-step (jenkins/install-plugins)
    (update-info destination-directory plugins)
  "Add plugins to a Jenkins installation, honoring plugin dependencies."
  (log:info "~@<Installing plugins with dependencies: ~{~A~^, ~}.~@:>"
            plugins)
  (let ((names-and-urls (collect-download-urls update-info plugins)))
    (ensure-directories-exist (jenkins-plugin-pathname
                               destination-directory "dummy"))
    (execute (make-step :jenkins/download-plugins) nil
             :destination-directory destination-directory
             :plugins               names-and-urls)))

;;; `jenkins/install-config-files' step

;;; For each configuration profile, load the configuration files into
;;; a resource group.
(map nil (lambda (directory)
           (let* ((name  (make-keyword
                          (string-upcase
                           (last-elt (pathname-directory directory)))))
                  (group (res:make-group name)))
             (res:add-file group directory)))
     (directory (merge-pathnames
                 #P"../../data/jenkins-install/config/*.*"
                 #.(or *compile-file-truename* *load-truename*))))

(defun profile-config-files (profile)
  (res:entries (res:find-group* profile)))

(define-sequence-step (jenkins/install-config-files
                       entry (config-files (profile-config-files profile))
                       :progress :install/config-file)
    (destination-directory
     (profile              :single-user))
  "Install Jenkins configuration files into a specified directory."
  (let+ (((&accessors-r/o (name                      res:name)
                          (content                   res:content)
                          ((&plist-r/o (mode :mode)) res:info))
          entry))
    (progress "~(~A~):~A" profile name)
    (let ((destination-file (merge-pathnames name destination-directory)))
      (log:info "~@<Installing ~A -> ~A with mode ~A~@:>"
                name destination-file mode)
      (ensure-directories-exist destination-file)
      (write-byte-vector-into-file content destination-file
                                   :if-exists :supersede)
      #+unix (sb-posix:chmod destination-file mode))))

;;; `jenkins/create-user' step

(let* ((base-directory (merge-pathnames
                        (make-pathname
                         :name      :unspecific
                         :type      :unspecific
                         :directory '(:relative :back :back "data" "jenkins-install"))
                        #.(or *compile-file-truename* *load-truename*)))
       (filename       (merge-pathnames
                        "user-config.xml.in" base-directory)))
  (res:add-file (res:make-group :user-configuration)
                filename :base-directory base-directory))

(macrolet ((define-xpath-constant (name xpath)
             `(define-constant ,name
                  `(xpath:xpath ,(xpath:parse-xpath ,xpath))
                :test #'equal)))

  (define-xpath-constant +jenkins-user-config-full-name-path+
    "/user/fullName/text()")

  (define-xpath-constant +jenkins-user-config-password-hash-path+
    "/user/properties/hudson.security.HudsonPrivateSecurityRealm_-Details/passwordHash/text()")

  (define-xpath-constant +jenkins-user-config-email-path+
    "/user/properties/hudson.tasks.Mailer_-UserProperty/emailAddress/text()"))

(defun jenkins-user-configuration-file (name destination-directory)
  (merge-pathnames (make-pathname :name      "config"
                                  :type      "xml"
                                  :directory `(:relative "users" ,name))
                   destination-directory))

(define-constant +jenkins-users-index-file+
  #P"users/users.xml"
  :test #'equalp)

(deftype user-id-map-entry/cons ()
  '(cons string string))

(defmethod xloc:xml-> ((value stp:element)
                       (type  (eql 'user-id-map-entry/cons))
                       &key inner-types)
  (declare (ignore inner-types))
  (xloc:with-locations-r/o ((name      "string[1]/text()")
                            (directory "string[2]/text()"))
      value
    (cons name directory)))

(defmethod xloc:->xml ((value cons)
                       (dest  stp:element)
                       (type  (eql 'user-id-map-entry/cons))
                       &key inner-types)
  (declare (ignore inner-types))
  (xloc:with-locations ((name      "string[1]/text()")
                        (directory "string[2]/text()"))
      dest
    (setf name (car value) directory (cdr value))))

(define-step (jenkins/create-user)
    (destination-directory
     (config-file-template (build-generator.resources:content
                            (build-generator.resources:find-entry
                             #P"user-config.xml.in"
                             (build-generator.resources:find-group* :user-configuration))))
     username email password)
  "Create a user in an existing Jenkins installation."
  (check-type username jenkins-username)
  (ensure-jenkins-directory-state '(:fresh :stopped) destination-directory)
  (with-trivial-progress (:install/user "~A" username)
    (let ((destination-file (jenkins-user-configuration-file
                             username destination-directory))
          (index-file       (merge-pathnames +jenkins-users-index-file+
                                             destination-directory)))

      ;; Add an entry for USERNAME in users.xml if it exists.
      (let ((document))
        (with-open-file (stream index-file
                                :element-type      '(unsigned-byte 8)
                                :direction         :input
                                :if-does-not-exist nil)
          (when stream
            (setf document (cxml:parse stream (stp:make-builder)))))
        (when document
          (xloc:with-locations (((:val entries :type 'user-id-map-entry/cons)
                                 "hudson.model.UserIdMapper/idToDirectoryNameMap/entry"
                                 :if-multiple-matches :all))
              document
            (let ((entry (find username entries :test #'string= :key #'car)))
              (cond ((not entry)
                     (appendf entries `((,username . ,username))))
                    (t
                     (error "~@<A user named \"~A\" already exists.~@:>" username)
                     (setf destination-file (jenkins-user-configuration-file
                                             (cdr entry) destination-directory))
                     (log:info "~@<New destination file ~S.~@:>"
                               destination-file)))))
          (with-output-to-file (stream index-file :element-type '(unsigned-byte 8)
                                                  :if-exists    :supersede)
            (cxml-stp:serialize document (cxml:make-octet-stream-sink stream)))))

      ;; Write user configuration file.
      (let* ((hash     (build-generator.bcrypt:hash-password password))
             (hash     (format nil "#jbcrypt:~A" hash))
             (document (cxml:parse config-file-template (stp:make-builder))))
        ;; Populate template.
        (mapc (lambda+ ((path . value))
                (xpath:do-node-set (node (xpath:evaluate path document))
                  (setf (stp:data node) value)))
              `((,+jenkins-user-config-full-name-path+     . ,username)
                (,+jenkins-user-config-password-hash-path+ . ,hash)
                (,+jenkins-user-config-email-path+         . ,email)))

        ;; Serialize modified document into the user configuration
        ;; file for USERNAME.
        (ensure-directories-exist destination-file)
        (with-output-to-file (stream destination-file
                                     :element-type '(unsigned-byte 8)
                                     :if-exists    :supersede)
          (stp:serialize document (cxml:make-octet-stream-sink stream)))))))
