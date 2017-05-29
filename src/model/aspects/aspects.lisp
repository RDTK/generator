;;;; aspects.lisp --- Aspect definitions
;;;;
;;;; Copyright (C) 2012-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.model.aspects)

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
  `(let ((,var (ensure-interface (,accessor ,object) (,class ,@initargs))))
     ,@body))

#.(interpol:enable-interpol-syntax)

;;; Description aspect

(define-aspect (description :job-var job) ()
    (((description (bail)) :type string
      :documentation
      "Textual description for the generated job.

       Depending on the global Jenkins configuration, the description
       is either interpreted as plain text or HTML."))
  "Adds a description to the generated job."
  (setf (description job) description))

;;; Parameters aspect

(define-aspect (parameters) ()
    ((parameters :type list
      :documentation
      "A list of parameters that should be configured for the
       generated job.

       Each entry has to be of one of the forms

         [ \"text\",   \"NAME\" ]
         [ \"text\",   \"NAME\", \"DEFAULT\" ]
         [ \"string\", \"NAME\" ]
         [ \"string\", \"NAME\", \"DEFAULT\" ]

       where \"text\" vs. \"string\" controls which values are
       acceptable for the parameter and NAME is the name of the
       parameter. Default values are optional.

       For more details, see Jenkins documentation."))
  "Adds parameters to the created job."
  (with-interface (properties job) (parameters* (property/parameters))
    (mapc (lambda+ ((kind name &optional default))
            (setf (parameters parameters*)
                  (remove name (parameters parameters*)
                          :key (rcurry #'getf :name)))
            (let ((kind (cond
                          ((string= kind "text")   :text)
                          ((string= kind "string") :string)
                          (t
                           (error "~@<Unsupported parameter kind: ~S.~@:>"
                                  kind)))))
              (push (list* :kind kind :name name
                           (when default (list :default default)))
                    (parameters parameters*))))
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
  (with-interface (properties job) (discard-builds (property/discard-builds))
    (setf (keep-builds/days     discard-builds) (or keep-builds/days     -1)
          (keep-builds/count    discard-builds) (or keep-builds/count    -1)
          (keep-artifacts/days  discard-builds) (or keep-artifacts/days  -1)
          (keep-artifacts/count discard-builds) (or keep-artifacts/count -1))))

;;; JDK aspect

(define-aspect (jdk :job-var job) ()
    (((jdk nil) :type (or null string)
      :documentation
      "Name of the JDK installation the generated job should use.

       A matching entry has to exist in Jenkins' global settings."))
  "Selects the JDK version to be used by the created job."
  (setf (jenkins.api::jdk job) jdk))

;;; Github aspect

(define-aspect (github :job-var job) ()
    (((project-url  nil) :type (or null string)
      :documentation
      "The URL of the GitHub page of the project.")
     ((display-name nil) :type (or null string)
      :documentation
      "The name that should be used in the link to the GitHub page."))
  "Configures an associated GitHub page for the generated job.

   The generated job will have a link to the GitHub page, links to
   commits in the GitHub repository viewer and other similar
   integration features."
  (if project-url
      (with-interface (properties job) (github (property/github))
        (setf (jenkins.api:project-url github) project-url
              (jenkins.api:display-name github) display-name))
      (removef (properties job) 'property/github :key #'type-of)))

;;; Redmine aspects

(define-aspect (redmine) ()
    (((instance (bail)) :type string
      :documentation
      "Redmine instance in which the project associated to the
       generated job resides.

       A matching entry has to exist in Jenkin's global settings.")
     ((project  (bail)) :type string
      :documentation
      "Name of the Redmine project to which the generated job should
       be associated."))
  "Configures Redmine integration for the created job.

   INSTANCE must refer to one of the Redmine instances in the global
   Jenkins configuration.

   It is recommended to use the full base URL of the actual Redmine as
   the respective name since the redmine-and-git aspect can reuse this
   information.  "
  (setf (jenkins.api::redmine-instance job) instance
        (jenkins.api::redmine-project job)  project))

(define-aspect (redmine-and-git
                :job-var     job
                :constraints ((:after aspect-git)))
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
  (let ((repository (repository job)))
    (unless (typep repository 'scm/git)
      (error "~@<Could not find git repository in ~A.~@:>" job))
    (setf (browser-kind repository) :redmine-web
          (browser-url  repository)
          (format nil "~A/projects/~A/repository/~@[~A/~]"
                  instance project repository-id))))

;;; Utilities for shell fragments

(defun make-remove-directory-contents/unix (&key exclude)
  (let ((exclude (ensure-list exclude)))
    (format nil "find . -mindepth 1 -maxdepth 1 ~
                        ~[~:*~;-not -name ~{~S~} ~:;-not \\( ~{-name ~S~^ -o ~} \\) ~]~
                        -exec rm -rf {} \\;"
            (length exclude) exclude)))

(defun make-move-stuff-upwards/unix (stuff)
  "Move contents of STUFF, which is a list of one or more directory
   name components, to the current directory."
  (declare (type list stuff))
  (let+ (((first &rest rest) stuff)
         (rest/string (namestring (make-pathname :directory `(:relative ,@rest)))))
    #?"# Uniquely rename directory.
temp=\$(mktemp -d ./XXXXXXXX)
mv -T \"${first}\" \"\${temp}/\"

# Move contents to toplevel workspace directory.
find \"\${temp}/${rest/string}\" -mindepth 1 -maxdepth 1 -exec mv {} . \\;
rm -rf \"\${temp}\""))

(defun make-variable/sh (string)
  (substitute-if #\_ (lambda (character)
                       (not (or (alphanumericp character)
                                (member character '(#\_)))))
                 string))

(defun wrap-shell-command (command pre post)
  (cond
    ((or pre post)
     (let+ (((rest &optional (shebang ""))
             (if (starts-with-subseq "#!" command)
                 (let ((index (position #\Newline command)))
                   (list (subseq command (1+ index)) (subseq command 0 (1+ index))))
                 (list command))))
       (concatenate 'string shebang (or pre "") rest (or post ""))))
    (t
     command)))

(defmacro wrapped-shell-command ((aspect-name
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
              (note-variable name type documentation)
              (values name `(load-time-value
                             (note-variable ,name ',type ,documentation))))))
         ((&values prefix-var-name prefix-var-form)
          (make-variable '#:.prefix "before"))
         ((&values suffix-var-name suffix-var-form)
          (make-variable '#:.suffix "after")))
    `(wrap-shell-command (progn
                           ,prefix-var-form
                           ,suffix-var-form
                           ,@body)
                         (value/cast aspect ,prefix-var-name nil)
                         (value/cast aspect ,suffix-var-name nil))))

;;; Timeout aspect

(define-aspect (timeout) ()
    ((timeout/minutes :type positive-integer
      :documentation
      "Number of minutes a build of the generated job can run before
       it should time out and fail."))
  "Adds a timeout for builds of the generated job."
  (with-interface (build-wrappers job) (timeout (build-wrapper/timeout))
    (setf (timeout/minutes timeout) timeout/minutes)))

;;; Slaves aspect

;; TODO separate slaves aspect for matrix-project jobs?
(define-aspect (slaves :job-var job) ()
    (((slaves             '()) :type list
      :documentation
      "A list of names of Jenkins slaves on which the job should be
       built.")
     ((restrict-to-slaves '()) :type (or null string)
      :documentation
      "A Jenkins \"label expression\" which restricts the generated
       job to slaves matching the expression.

       See description of option \"Advanced Project Options\" »
       \"Restrict where this project can be run\" in Jenkins' job
       configuration for details."  ))
  "Configures the generated job to run on specific slaves."
  (when slaves
    (setf (slaves job) slaves))
  (if restrict-to-slaves
      (setf (can-roam? job)          nil
            (restrict-to-slaves job) restrict-to-slaves)
      (setf (can-roam? job) t)))

;;; Permissions aspect

(define-aspect (permissions :job-var job) ()
    (((permissions :keep) :type (or (eql :keep) list)
      :documentation
      "The permissions to install.

       A list of entries of the form

         [ \"SUBJECT\", [ \"ACTION-NAME₁\", \"ACTION-NAME₂\", … ] ]

       where SUBJECT is typically the username for whom permissions
       are installed and ACTION-NAMEN are the components of the name
       of the action that is being permitted for SUBJECT.

       Example:

         [ [ \"jdoe\", [ \"item\", \"build\"  ],
           [ \"jdoe\", [ \"item\", \"cancel\" ] ]

       ."))
  "Configures user and group permissions for the generated job."
  (let+ (((&flet+ normalize-permission ((subject action))
            (list subject (mapcar (compose #'make-keyword #'string-upcase)
                                  action)))))
    (unless (eq permissions :keep)
      (setf (permissions job) (mapcar #'normalize-permission permissions)))))

#.(interpol:disable-interpol-syntax)
