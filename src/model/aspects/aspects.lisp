;;;; aspects.lisp --- Aspect definitions
;;;;
;;;; Copyright (C) 2012-2022 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:build-generator.model.aspects)

;; TODO(jmoringe, 2013-02-21): find better location for these macros
(defmacro ensure-interface ((accessor object) (class &rest initargs))
  (once-only (object)
    (with-gensyms (implementation)
      `(or (find-if (of-type ',class) (,accessor ,object))
           (let ((,implementation (make-instance ',class ,@initargs)))
             (appendf (,accessor ,object) (list ,implementation))
             ,implementation)))))

(defmacro with-interface ((accessor object) (var (class &rest initargs))
                          &body body)
  (let ((ensure `(ensure-interface (,accessor ,object) (,class ,@initargs))))
    `(,@ (if var
             `(let ((,var ,ensure)))
             `(progn ,ensure))
         ,@body)))

;;; Description aspect

(define-aspect (description :job-var job) ()
    (((description (bail)) :type string
      :documentation
      "Textual description for the generated job.

       Depending on the global Jenkins configuration, the description
       is either interpreted as plain text or HTML."))
  "Adds a description to the generated job."
  (setf (jenkins.api:description job) description))

;;; Parameters aspect

(deftype parameter-entry/legacy ()
  '(cons string (cons string (or null (cons string null)))))

(defun every-parameter-entry/legacy (thing)
  (and (listp thing) (every (of-type 'parameter-entry/legacy) thing)))

(defun parameter-entry? (thing)
  (nth-value 1 (var:as thing 'parameter-entry :if-type-mismatch nil)))

(deftype parameter-entry ()
  '(and cons (satisfies parameter-entry?)))

(defun every-parameter-entry (thing)
  (and (listp thing) (every (of-type 'parameter-entry) thing)))

(defmethod var:as ((value t) (type (eql 'parameter-entry))
                   &key if-type-mismatch)
  (declare (ignore if-type-mismatch))
  (when (and (consp value)
             (every (lambda (cell)
                      (and (consp cell)
                           (member (car cell)
                                   '(:name :kind :default :description))))
                    value))
    (when-let* ((name (assoc-value value :name))
                (kind (assoc-value value :kind))
                (kind (var:as kind '(or (eql :text)
                                        (eql :string)
                                        (eql :boolean)
                                        (eql :password))
                              :if-type-mismatch nil)))
      (let ((default (assoc-value value :default)))
        (cond ((and (eq kind :password) default)
               nil)
              (t
               (values
                (list* :name name
                       :kind kind
                       (append (when default
                                 `(:default ,default))
                               (when-let ((description (assoc-value value :description)))
                                 `(:description ,description))))
                t)))))))

(defmethod var:as ((value t) (type (eql 'parameter-entry/legacy))
                   &key if-type-mismatch)
  (declare (ignore if-type-mismatch))
  (when (typep value '(cons string))
    (log:warn "~@<Parameter specification uses legacy format: ~S.~@:>"
              (json:encode-json-to-string value))
    (destructuring-bind (kind name &optional (default nil default?))
        value
      (when-let ((kind (var:as kind '(or (eql :text) (eql :string)))))
        (values (list* :kind kind :name name
                       (when default?
                         (list :default default)))
                t)))))

(assert (equal (var:as '((:name . "foo") (:kind . "text")) 'parameter-entry)
               '(:name "foo" :kind :text)))
(assert (equal (var:as '(((:name . "foo") (:kind . "text"))) '(var:list-of parameter-entry))
               '((:name "foo" :kind :text))))

(assert (typep '(("string" "ageLimit" "none")) '(var:list-of parameter-entry/legacy)))
(assert (not (typep '(("string" "ageLimit" "none")) '(var:list-of parameter-entry))))

(assert (not (typep '(((:kind . "string") (:name . "ageLimit") (:default . "none")))
                    '(var:list-of parameter-entry/legacy))))
(assert (typep '(((:kind . "string") (:name . "ageLimit") (:default . "none")))
               '(var:list-of parameter-entry)))

(define-aspect (parameters) ()
    ((parameters :type (or (var:list-of parameter-entry)
                           (var:list-of parameter-entry/legacy))
      :documentation
      "A list of parameters that should be configured for the
       generated job.

       Each entry has to be of one of the forms

         name:        NAME
         kind:        KIND
         default:     DEFAULT
         description: DESCRIPTION

       where

       NAME is the name of the parameter.

       KIND is one of \"text\", \"string\", \"boolean\" or
       \"password\" and controls which values are acceptable for the
       parameter and NAME is the name of the parameter.

       DEFAULT is optional and, if present, specifies the default
       value of the parameter. The kind \"password\" does not allow a
       default value.

       DESCRIPTION is optional and, if present, specifies a
       description Jenkins should present alongside the parameter.

       For more details, see Jenkins documentation."))
  "Adds parameters according to PARAMETERS to the created job."
  (with-interface (jenkins.api:properties job)
      (parameters* (jenkins.api:property/parameters))
    (mapc (lambda+ ((&whole spec &key name &allow-other-keys))
            (setf (jenkins.api:parameters parameters*)
                  (list* spec (remove name (jenkins.api:parameters parameters*)
                                      :key (rcurry #'getf :name)))))
          parameters)))

;;; Retention aspect

(define-aspect (retention :job-var job) ()
    ((keep-builds/days     :type (or null positive-integer)
      :documentation
      "Number of days for which past builds of the generated job
       should be kept.

       \"false\" to keep builds indefinitely.")
     (keep-builds/count    :type (or null positive-integer)
      :documentation
      "Number of past build that should be kept for the generated
       job.

       \"false\" to keep an unlimited number of builds.")
     (keep-artifacts/days  :type (or null positive-integer)
      :documentation
      "Number of days for which artifacts of past builds of the
       generated job should be kept.

       \"false\" to keep artifacts as long as the associated jobs.")
     (keep-artifacts/count :type (or null positive-integer)
      :documentation
      "Number of past builds of the generated job for which artifacts
       should be kept.

       \"false\" to keep an unlimited number of builds with
       artifacts."))
  "Configures retention of builds and artifacts for the created job."
  (with-interface (jenkins.api:properties job)
      (discard-builds (jenkins.api:property/discard-builds))
    (setf (jenkins.api:keep-builds/days     discard-builds) (or keep-builds/days     -1)
          (jenkins.api:keep-builds/count    discard-builds) (or keep-builds/count    -1)
          (jenkins.api:keep-artifacts/days  discard-builds) (or keep-artifacts/days  -1)
          (jenkins.api:keep-artifacts/count discard-builds) (or keep-artifacts/count -1))))

;;; JDK aspect

(define-aspect (jdk :job-var job) ()
    (((jdk nil) :type (or null string)
      :documentation
      "Name of the JDK installation the generated job should use.

       A matching entry has to exist in Jenkins' global settings."))
  "Selects the JDK version to be used by the created job."
  (setf (jenkins.api::jdk job) jdk))

;;; Github aspect

(define-aspect (github :job-var job
                       :plugins ("github"))
    ()
    (((project-url  nil) :type (or null string)
      :documentation
      "The URL of the GitHub page of the project.")
     ((display-name nil) :type (or null string)
      :documentation
      "The name that should be used in the link to the GitHub page."))
  "Configures an associated GitHub page for the generated job.

   The generated job will have a link to the GitHub page at
   PROJECT-URL, links to commits in the GitHub repository viewer and
   other similar integration features."
  (if project-url
      (with-interface (jenkins.api:properties job)
          (github (jenkins.api:property/github))
        (setf (jenkins.api:project-url github) project-url
              (jenkins.api:display-name github) display-name))
      (removef (jenkins.api:properties job) 'property/github :key #'type-of)))

;;; Redmine aspects

(define-aspect (redmine :job-var job
                        :plugins ("redmine"))
    ()
    (((instance nil) :type (or null string)
      :documentation
      "Redmine instance in which the project associated to the
       generated job resides.

       A matching entry has to exist in Jenkin's global settings.")
     ((project  nil) :type (or null string)
      :documentation
      "Name of the Redmine project to which the generated job should
       be associated."))
  "Configures Redmine integration with PROJECT for the created job.

   INSTANCE must refer to one of the Redmine instances in the global
   Jenkins configuration.

   It is recommended to use the full base URL of the actual Redmine as
   the respective name since the redmine-and-git aspect can reuse this
   information."
  (if (and instance project)
      (with-interface (jenkins.api:properties job)
          (redmine (jenkins.api:property/redmine))
        (setf (jenkins.api:instance     redmine) instance
              (jenkins.api:project-name redmine) project))
      (removef (jenkins.api:properties job) 'property/redmine :key #'type-of)))

(define-aspect (redmine-and-git
                :job-var     job
                :constraints ((:after aspect-git))
                :plugins     ("git" "redmine"))
    ()
    (((instance      (bail)) :type string
      :documentation
      "Redmine instance in which the project associated to the
       generated job resides.

       A matching entry has to exist in Jenkin's global settings.")
     ((project       (bail)) :type string
      :documentation
      "Name of the Redmine project in which the repository accessed by
       the generated job is contained.")
     ((repository-id (bail)) :type string
      :documentation
      "Name of the repository within the Redmine project."))
  "Configures integration of Redmine and git repositories.

   The aspect only works correctly if the generated job is configured
   with a git repository."
  (let ((repository (jenkins.api:repository job)))
    (unless (typep repository 'jenkins.api:scm/git)
      (error "~@<Could not find git repository in ~A.~@:>" job))
    (setf (jenkins.api:browser-kind repository) :redmine-web
          (jenkins.api:browser-url  repository)
          (format nil "~A/projects/~A/repository/~@[~A/~]"
                  instance project repository-id))))

;;; Utilities for shell fragments

(defun make-remove-directory-contents/unix (&key exclude)
  (let ((exclude (ensure-list exclude)))
    (format nil "find . -mindepth 1 -maxdepth 1 ~
                        ~[~:*~;-not -name ~{~S~} ~:;-not \\( ~{-name ~S~^ -o ~} \\) ~]~
                        -exec rm -rf {} \\;"
            (length exclude) exclude)))

(defun+ make-move-stuff-upwards/unix ((first &rest rest))
  "Move contents of FIRST and REST, which form a list of one or more
   directory name components, to the current directory."
  (format nil "# Uniquely rename directory.~@
               temp=$(mktemp -d ./XXXXXXXX)~@
               mv -T \"~A\" \"${temp}/\"~@
               ~@
               # Move contents to toplevel workspace directory.~@
               find \"${temp}/~{~A~^/~}\" -mindepth 1 -maxdepth 1 -exec mv {} . \\;~@
               rm -rf \"${temp}\""
          first rest))

(defun make-variable/sh (string)
  (substitute-if #\_ (lambda (character)
                       (not (or (alphanumericp character)
                                (member character '(#\_)))))
                 string))

(defun wrap-shell-command (stream command pre post)
  (flet ((do-it (stream)
           (if (or pre post)
               (destructuring-bind (rest &optional (shebang ""))
                   (if (starts-with-subseq "#!" command)
                       (let ((index (position #\Newline command)))
                         (list (subseq command (1+ index)) (subseq command 0 (1+ index))))
                       (list command))
                 (write-string shebang stream)
                 (when pre (write-string pre stream))
                 (write-string rest stream)
                 (when post (write-string post stream)))
               (write-string command stream))))
    (if (null stream)
        (with-output-to-string (stream) (do-it stream))
        (do-it stream))))

(defmacro as-shell-command ((stream aspect-name
                             &optional (builder-name '#:command))
                            &body body)
  (let+ (((&flet make-variable (suffix when)
            (let ((name (let ((*package* (find-package '#:keyword)))
                          (symbolicate aspect-name '#:. builder-name suffix)))
                  (type '(or null string))
                  (documentation
                   (format nil "Shell fragment to execute ~A the shell ~
                                fragment of the ~A aspect."
                           when aspect-name)))
              (var:note-variable name type :documentation documentation)
              (values name `(load-time-value
                             (var:note-variable ,name ',type :documentation ,documentation))))))
         ((&values prefix-var-name prefix-var-form)
          (make-variable '#:.prefix "before"))
         ((&values suffix-var-name suffix-var-form)
          (make-variable '#:.suffix "after")))
    `(wrap-shell-command ,stream
                         (progn
                           ,prefix-var-form
                           ,suffix-var-form
                           ,@body)
                         (var:value/cast aspect ,prefix-var-name nil)
                         (var:value/cast aspect ,suffix-var-name nil))))

;;; Timeout aspect

(define-aspect (timeout :plugins ("build-timeout"))
    ()
    ((timeout/minutes :type positive-integer
      :documentation
      "Number of minutes a build of the generated job can run before
       it should time out and fail."))
  "Adds a timeout for builds of the generated job."
  (with-interface (jenkins.api:build-wrappers job)
      (timeout (jenkins.api:build-wrapper/timeout))
    (setf (jenkins.api:timeout/minutes timeout) timeout/minutes)))

;;; sonar cube scanner aspect

(define-aspect (sonar-cube-scanner :plugins ("sonar"))
    ()
    ((mode :type (or (eql :inject) (eql :explicit))
      :documentation
      "How should the SonarCube Scanner be invoked?

       inject: Insert a wrapper that defines environment variables to
       be picked up by Maven or Gradle invocations.

       explicit: Insert a build step that invokes the scanner
       explicitly."))
  "Adds SonarCube Scanner configuration."
  (ecase mode
    (:inject
     (with-interface (jenkins.api:build-wrappers job)
         (nil (jenkins.api:build-wrapper/sonar))))
    (:explicit
     (error "Not implemented."))))

;;; Console Timestamper aspect

(define-aspect (console-timestamper :plugins ("timestamper")) ()
    ()
  "Configures the console timestamper."
  (with-interface (jenkins.api:build-wrappers job)
      (nil (jenkins.api:build-wrapper/timestamper))))

;;; ANSI color aspect

(define-aspect (console-ansi-color :plugins ("ansicolor")) ()
    (((color-map "xterm") :type string
       :documentation
       "Name of the color map the plugin should use."))
  "Adds console ANSI color support to the generated job."
  (with-interface (jenkins.api:build-wrappers job)
      (nil (jenkins.api:build-wrapper/ansi-color
            :color-map color-map))))

;;; Slaves aspect

;; TODO separate slaves aspect for matrix-project jobs?
(define-aspect (slaves :job-var job) ()
    (((slaves             '()) :type (var:list-of string)
      :documentation
      "A list of names of Jenkins slaves on which the job should be
       built.")
     ((restrict-to-slaves '()) :type (or null string)
      :documentation
      "A Jenkins \"label expression\" which restricts the generated
       job to slaves matching the expression.

       See description of option \"Advanced Project Options\" »
       \"Restrict where this project can be run\" in Jenkins' job
       configuration for details."))
  "Configures the generated job to run on specific slaves."
  (when slaves
    (setf (jenkins.api:slaves job) slaves))
  (if restrict-to-slaves
      (setf (jenkins.api:can-roam? job)          nil
            (jenkins.api:restrict-to-slaves job) restrict-to-slaves)
      (setf (jenkins.api:can-roam? job) t)))

;;; Permissions aspect

(deftype permission-entry ()
  '(cons string (cons (var:list-of string) null)))

(defun every-permission-entry (thing)
  (and (listp thing) (every (of-type 'permission-entry) thing)))

(define-aspect (permissions :job-var job) ()
    (((permissions :keep) :type (or (eql :keep) (var:list-of permission-entry))
      :documentation
      "The permissions to install.

       A list of entries of the form

         [ \"SUBJECT\", [ \"ACTION-NAME₁\", \"ACTION-NAME₂\", … ] ]

       where SUBJECT is typically the username for whom permissions
       are installed and ACTION-NAMEₖ are the components of the name
       of the action that is being permitted for SUBJECT.

       Example:

         [ [ \"jdoe\", [ \"item\", \"build\"  ],
           [ \"jdoe\", [ \"item\", \"cancel\" ] ]

       ."))
  "Configures user and group permissions for the generated job."
  (let+ (((&flet+ normalize-permission ((subject action))
            (let ((action (mapcar (compose #'make-keyword #'string-upcase)
                                  action)))
              (list subject action :kind :user)))))
    (unless (eq permissions :keep)
      (setf (jenkins.api:permissions job)
            (mapcar #'normalize-permission permissions)))))
