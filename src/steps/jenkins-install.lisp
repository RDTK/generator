;;;; jenkins-install.lisp --- Steps for setting up a Jenkins instance.
;;;;
;;;; Copyright (C) 2015, 2016, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.project.steps)

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
           :running))))

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

(define-constant +default-jenkins-download-url+
    (puri:uri "http://mirrors.jenkins-ci.org/war-stable/latest/jenkins.war")
  :test #'puri:uri=)

(define-step (jenkins/install-core)
    ((url                  +default-jenkins-download-url+)
     destination-directory)
  "Download the Jenkins archive into a specified directory."
  (ensure-jenkins-directory-state :not-present destination-directory)
  (let ((pathname (merge-pathnames "jenkins.war" destination-directory)))
    (ensure-directories-exist pathname)
    (when (not (probe-file pathname))
      (analysis::download-file url pathname))))

;;; `jenkins/install-plugins[-with-dependencies]' steps

(define-constant +jenkins-plugin-directory+
  #P"plugins/"
  :test #'equalp)

(define-constant +jenkins-plugin-manifest-filename+
  "META-INF/MANIFEST.MF"
  :test #'string=)

(define-constant +jenkins-plugins-base-url+
    (puri:uri "https://updates.jenkins-ci.org/latest/")
  :test #'puri:uri=)

(defun jenkins-plugin-pathname (base-directory name)
  (merge-pathnames (make-pathname :name     name
                                  :type     "hpi"
                                  :defaults +jenkins-plugin-directory+)
                   base-directory))

(defun jenkins-plugin-url (name &key (base-url +jenkins-plugins-base-url+))
  (puri:merge-uris (format nil "~A.hpi" name) base-url))

(defgeneric jenkins-plugin-dependencies (thing)
  (:method ((thing string))
    (let+ ((clean (ppcre:regex-replace-all
                   #.(format nil "~C~%(:? )?" #\Return) thing
                   (lambda (whole space)
                     (declare (ignore whole))
                     (if (emptyp space) (string #\Newline) ""))
                   :simple-calls t))
           ((&flet parse-dependency (spec)
              (ppcre:register-groups-bind (name version optional?)
                  ("([^:]+):([^;]+)(;resolution:=optional)?" spec)
                (list name version (when optional? t))))))
      (ppcre:register-groups-bind (dependencies)
          ("Plugin-Dependencies: +(.+)" clean)
        (mapcar #'parse-dependency
                (split-sequence:split-sequence #\, dependencies)))))
  (:method ((thing pathname))
    (jenkins-plugin-dependencies
     (zip:with-zipfile (zip thing)
       (let ((manifest (zip:get-zipfile-entry +jenkins-plugin-manifest-filename+ zip)))
         (sb-ext:octets-to-string (zip:zipfile-entry-contents manifest)))))))

(define-sequence-step (jenkins/install-plugins plugin plugins
                       :execution :parallel
                       :progress  nil)
    (destination-directory)
  "Add specified plugins to an existing Jenkins installation."
  (progress :install/plugin nil "~A" plugin)
  (let ((pathname (jenkins-plugin-pathname destination-directory plugin))
        (url      (jenkins-plugin-url plugin)))
    (unless (probe-file pathname)
      (log:info "~@<Installing ~A from ~A into ~A.~@:>" plugin url pathname)
      (analysis::download-file url pathname)
      (jenkins-plugin-dependencies pathname))))

(define-step (jenkins/install-plugins-with-dependencies)
    (destination-directory plugins)
  "Add plugins to a Jenkins installation, honoring plugin dependencies."
  (let+ ((step      (make-step :jenkins/install-plugins))
         (installed '())
         ((&flet install (round plugins)
            (log:info "~@<Installing plugins with dependencies round ~
                       ~D: ~{~A~^, ~}.~@:>"
                      round plugins)
            (appendf installed plugins)
            (let* ((dependencies (execute
                                  step :context
                                  :destination-directory destination-directory
                                  :plugins               plugins))
                   (dependencies (mapcar #'first
                                         (remove-if #'third
                                                    (mappend #'identity dependencies))))
                   (dependencies (remove-duplicates dependencies :test #'string=)))
              (set-difference dependencies installed :test #'string=)))))
    (with-trivial-progress (:install/plugin)
      (ensure-directories-exist (jenkins-plugin-pathname
                                 destination-directory "dummy"))
      (loop :for i :from 0
            :for missing = plugins :then (install i missing)
            :while missing))))

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
     (config-file-template (jenkins.project.resources:content
                            (jenkins.project.resources:find-entry
                             #P"user-config.xml.in"
                             (jenkins.project.resources:find-group* :user-configuration))))
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
      (let* ((hash     (jenkins.project.bcrypt:hash-password password))
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
