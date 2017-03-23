;;;; aspects.lisp --- Aspect definitions
;;;;
;;;; Copyright (C) 2012-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.model.aspects)

;; TODO(jmoringe, 2013-02-21): find better location for these macros
(defmacro ensure-interface ((accessor object) (class &rest initargs))
  "TODO(jmoringe): document"
  (once-only (object)
    (with-gensyms (implementation)
      `(or (find-if (of-type ',class) (,accessor ,object))
           (let ((,implementation (make-instance ',class ,@initargs)))
             (appendf (,accessor ,object) (list ,implementation))
             ,implementation)))))

(defmacro with-interface ((accessor object) (var (class &rest initargs))
                          &body body)
  "TODO(jmoringe): document"
  `(let ((,var (ensure-interface (,accessor ,object) (,class ,@initargs))))
     ,@body))

#.(interpol:enable-interpol-syntax)

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
    ((keep/days  :type (or null positive-integer)
      :documentation
      "Number of days for which past builds of the generated job
       should be kept.")
     (keep/count :type (or null positive-integer)
      :documentation
      "Number of past build that should be kept for the generated
       job."))
  "Configures the retention of old builds for the created job."
  (setf (keep/days  job) keep/days
        (keep/count job) keep/count))

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

;;; SCM aspects

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
  (let ((prefix-var-name (let ((*package* (find-package '#:keyword)))
                           (symbolicate aspect-name '#:. builder-name '#:.prefix)))
        (suffix-var-name (let ((*package* (find-package '#:keyword)))
                           (symbolicate aspect-name '#:. builder-name '#:.suffix))))
    `(let ((prefix (as (value aspect ,prefix-var-name nil) '(or null string)))
           (suffix (as (value aspect ,suffix-var-name nil) '(or null string))))
       (wrap-shell-command (progn ,@body) prefix suffix))))

(defun split-option (spec)
  (let ((position (position #\= spec)))
    (unless position
      (error "~@<Option ~S is not of the form NAME=VALUE.~@:>"
             spec))
    (list (subseq spec 0 position) (subseq spec (1+ position)))))

(define-aspect (archive) (builder-defining-mixin)
    ((url            :type string
     :documentation
     "URL from which the archive should be downloaded.

      HTTP and HTTPS are supported.")
     ((filename nil) :type string
      :documentation
      "Name of the file in which the downloaded archive should be
       stored."))
  "Adds a build step that downloads a source archive.

   This may be useful when a SCM repository is not available but
   source archives are."
  ;; In case we are updating an existing job, remove any repository
  ;; configuration.
  (setf (repository job) (make-instance 'scm/null))

  ;; Generate archive download and extraction as a shell builder.
  (let* ((url/parsed (puri:uri url))
         (archive    (or filename (lastcar (puri:uri-parsed-path url/parsed)))))
    (push (constraint! (build ((:before t)))
            (shell (:command #?"# Clean workspace.
${(make-remove-directory-contents/unix)}

# Unpack archive.
wget --no-verbose \"${url}\" --output-document=\"${archive}\"
unp -U \"${archive}\"
rm \"${archive}\"
directory=\$(find . -mindepth 1 -maxdepth 1)

${(make-move-stuff-upwards/unix '("${directory}"))}")))
          (builders job))))

(define-aspect (git :job-var job :aspect-var aspect) (builder-defining-mixin)
    ((url                                   :type string
      :documentation
      "URL of the remote git repository from which the project source
       should be cloned.")
     ((username                       nil)  :type string
      :documentation
      "Username that should be used when cloning the remote repository.

       Should rarely be necessary since the credentials parameter can
       is often more appropriate.")
     ((password                       nil)  :type string
      :documentation
      "Password to use when cloning the remote repository.

       HUGE SECURITY RISK. Use the credentials parameter instead.")
     ((credentials                    nil)  :type string
      :documentation
      "Name of an entry in Jenkins' global credentials store that
       should be use for authenticating against the remote repository
       server.")
     (branches                              :type list #|of string|#
      :documentation
      "List of names of branches in the git repository that should be
       checked out.")
     ((local-branch                   nil)  :type string)
     ((clone-timeout                  nil)  :type positive-integer
      :documentation
      "Timeout for the git clone operation in minutes.")
     ((wipe-out-workspace?            nil)  :type boolean
      :documentation
      "Controls whether the entire workspace, including previously
       cloned git repositories, should be deleted before the build,
       thus forcing a fresh clone and new build from scratch.

       Not recommended unless a project does not build without it.")
     ((clean-before-checkout?         t)    :type boolean
      :documentation
      "Delete all files in the workspace that are not under version
       control before building.

       Can be used to force building from scratch without deleting the
       cloned repository, thus enabling incremental updates instead of
       fresh clones for every build.")
     ((checkout-submodules?           nil)  :type boolean)
     ((shallow?                       nil)  :type boolean)
     (((:sub-directory sub-directory) nil)  :type string))
  "Configures a GIT repository in the generated job.

   If USERNAME and PASSWORD are supplied, the supplied values may show
   up in the configuration and/or build logs of the generated
   job. Handle with care.

   If CREDENTIALS is supplied, a corresponding entry has to be created
   in the global Jenkins credentials configuration."
  ;; Configure GIT scm plugin.
  (let* ((url/parsed  (puri:uri url))
         (credentials (or credentials
                          (unless (check-access aspect :public)
                            (puri:uri-host url/parsed)))))
    (setf (repository job)
          (git (:url                    (jenkins.analysis::format-git-url
                                         url/parsed username password)
                :credentials            credentials
                :branches               branches
                :clone-timeout          clone-timeout
                :wipe-out-workspace?    wipe-out-workspace?
                :clean-before-checkout? clean-before-checkout?
                :checkout-submodules?   checkout-submodules?
                :shallow?               shallow?
                :local-branch           local-branch
                :internal-tag?          nil))))

  ;; If a specific sub-directory of the repository has been requested,
  ;; move the contents of that sub-directory to the top-level
  ;; workspace directory before proceeding.
  (when sub-directory
    (let+ ((sub-directory (uiop:ensure-directory-pathname sub-directory))
           ((&whole components first &rest &ign)
            (rest (pathname-directory sub-directory))))
      (push (constraint! (build ((:before t)))
              (shell (:command #?"${(make-remove-directory-contents/unix
                                     :exclude (list ".git" first))}

${(make-move-stuff-upwards/unix components)}")))
            (builders job)))))

(define-aspect (git-repository-browser
                :job-var     job
                :constraints ((:after aspect-git)))
    ()
    (((kind (bail)) :type keyword)
     ((url  (bail)) :type string))
  "Configures the GIT-specific repository browser of the generated job "
  (let ((repository (repository job)))
    (unless (typep repository 'scm/git)
      (error "~@<Could not find git repository in ~A.~@:>" job))
    (setf (browser-kind repository) kind
          (browser-url  repository) url)))

(define-aspect (subversion :job-var job :aspect-var aspect) ()
    ((url                             :type string
     :documentation
     "URL from which the checkout should be performed including,
      tag/branch path and optionally sub-directory.")
     ((revision          nil)         :type string
      :documentation
      "A subversion revision that should be checked out instead of the
       newest revision of the specified path.")
     ((credentials       nil)         :type string
      :documentation
      "Name of an entry in Jenkins' global credentials store that
       should be use for authenticating against the remote repository
       server.")
     (local-dir                       :type string)
     ((checkout-strategy :fresh-copy) :type string)) ; TODO better type
  "Configures a Subversion repository in the generated job.

   If CREDENTIALS is supplied, a corresponding entry has to be created
   in the global Jenkins credentials configuration."
  (let* ((url/parsed   (puri:uri url))
         (url/parsed   (puri:copy-uri
                        url/parsed
                        :path (ppcre:regex-replace-all
                               "//+" (puri:uri-path url/parsed) "/")))
         (url/revision (format nil "~A~@[@~A~]" url/parsed revision))
         (credentials  (or credentials
                           (unless (check-access aspect :public)
                             (puri:uri-host url/parsed)))))
    (setf (repository job)
          (svn (:url               url/revision
                :credentials       credentials
                :local-directory   local-dir
                :checkout-strategy (make-keyword
                                    (string-upcase
                                     checkout-strategy)))))))

(define-aspect (mercurial :job-var job :aspect-var aspect)
    (builder-defining-mixin)
    ((url                                  :type string
      :documentation
      "URL of the remote mercurial repository from which the project
       source should be cloned.")
     ((credentials                    nil) :type string
      :documentation
      "Name of an entry in Jenkins' global credentials store that
       should be use for authenticating against the remote repository
       server.")
     ((branch                         nil) :type string
      :documentation
      "Name of the branch in the mercurial repository that should be
       checked out.

       Mutually exclusive with the tag parameter.")
     ((tag                            nil) :type string
      :documentation
      "Name of the tag in the mercurial repository that should be
       checked out.

       Mutually exclusive with the branch parameter.")
     (clean?                                :type boolean
      :documentation
      "Controls whether the workspace is cleaned before each build.")
     (((:sub-directory sub-directory) nil) :type string))
  "Configures a Mercurial repository in the generated job.

   If CREDENTIALS is supplied, a corresponding entry has to be created
   in the global Jenkins credentials configuration."
  ;; Configure mercurial scm plugin.
  (let* ((url/parsed   (puri:uri url))
         (credentials  (or credentials
                           (unless (check-access aspect :public)
                             (puri:uri-host url/parsed)))))
    (when (and branch tag)
      (error "~@<Cannot specify branch ~S and tag ~S at the same time.~@:>"
             branch tag))
    (setf (repository job)
          (mercurial (:url           url
                      :credentials   credentials
                      :revision-type (cond
                                       (branch :branch)
                                       (tag    :tag))
                      :branch        (or branch tag)
                      :clean?        clean?))))

  ;; TODO mercurial seems to support sub-directories
  ;; If a specific sub-directory of the repository has been requested,
  ;; move the contents of that sub-directory to the top-level
  ;; workspace directory before proceeding.
  (when sub-directory
    (let+ ((sub-directory (uiop:ensure-directory-pathname sub-directory))
           ((&whole components first &rest &ign)
            (rest (pathname-directory sub-directory))))
      (push (constraint! (build ((:before t)))
              (shell (:command #?"${(make-remove-directory-contents/unix
                                     :exclude (list ".hg" first))}

${(make-move-stuff-upwards/unix components)}")))
            (builders job)))))

(define-aspect (trigger/scm) ()
    ((spec :type (or null string)
      :documentation
      "Describes the schedule Jenkins should employ for polling the
       repository.

       See Jenkins documentation for details."))
  "Configures the generated job such that it polls the SCM repository."
  (removef (triggers job) 'trigger/scm :key #'type-of)
  (when spec
    (push (scm (:spec spec)) (triggers job))))

;;; Timeout aspect

(define-aspect (timeout) ()
    ((timeout/minutes :type positive-integer
      :documentation
      "Number of minutes a build of the generated job can run before
       it should time out and fail."))
  "Adds a timeout for builds of the generated job."
  (with-interface (build-wrappers job) (timeout (build-wrapper/timeout))
    (setf (timeout/minutes timeout) timeout/minutes)))

;;; Tasks aspect

(define-aspect (tasks) (publisher-defining-mixin)
  ((pattern               :type list #|of string|#
    :documentation
    "Filename patterns specifying which workspace files to scan for
     open tasks.")
   ((exclude         '()) :type list #|of string|#
    :documentation
    "Filename patterns specifying which workspace files to exclude
     from the scan for open tasks.")
   ((keywords.low    '()) :type list #|of string|#
    :documentation
    "Keywords indicating low-priority open tasks.")
   ((keywords.normal '()) :type list #|of string|#
    :documentation
    "Keywords indicating normal-priority open tasks.")
   ((keywords.high   '()) :type list #|of string|#
    :documentation
    "Keywords indicating high-priority open tasks."))
  "Adds an open tasks publisher to the generated job."
  (push (constraint! (publish)
          (tasks (:pattern         pattern
                  :exclude         exclude
                  :keywords/low    keywords.low
                  :keywords/normal keywords.normal
                  :keywords/high   keywords.high)))
        (publishers job)))

;;; SLOCcount aspect

(define-aspect (sloccount) (builder-defining-mixin
                            publisher-defining-mixin)
    ((directories :type list))
  "Adds a sloccount publisher to the generated job."
  (let ((arguments (mapcar #'prin1-to-string directories)))
    (push (constraint! (build ((:before cmake/unix)
                               (:before maven)
                               (:before setuptools)))
            (shell (:command #?"DATA_DIR=\$(mktemp -d /tmp/build-generator.sloccount.data.XXXXXXXXXX)
REPORT_DIR=\$(mktemp -d /tmp/build-generator.sloccount.report.XXXXXXXXXX)
mkdir -p \"\${REPORT_DIR}\"
sloccount --datadir \"\${DATA_DIR}\" --wide --details @{arguments} > \"\${REPORT_DIR}/sloccount.sc\"
mv \"\${REPORT_DIR}/sloccount.sc\" \"\${WORKSPACE}/sloccount.sc\"
rm -rf \"\${DATA_DIR}\" \"\${REPORT_DIR}\"")))
          (builders job)))

   (push (constraint! (publish) (sloccount (:pattern "sloccount.sc")))
        (publishers job)))

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

;;; Dependency download aspect

(define-aspect (dependency-download :job-var  job
                                    :spec-var spec)
    (builder-defining-mixin)
    ((((:upstream-dir upstream-dir)) :type string))
  "Configures artifact downloads for the generated job.

   Copy-artifact actions are generated to copy artifacts from all
   suitable jobs generated for projects in the transitive
   upstream-closure (i.e. upstream projects, upstream projects of
   upstream projects, etc.). A job from this set is suitable if it is
   configured to archive any artifacts. Such artifacts will be copied
   into the directory designated by the upstream-dir parameter and
   extracted.

   Note: An error is signaled if an attempt is made to copy artifacts
   from a matrix upstream project into a non-matrix downstream
   project. The combinations
     matrix     -> matrix
     non-matrix -> matrix
     non-matrix -> non-matrix
   are permitted.

   Note: Only useful in continuous integration operation modes that do
   not install built software but copy results from upstream jobs into
   the workspaces of downstream jobs."
   (let ((copy-artifacts? nil))
     (when-let ((self-kind    (first (ensure-list (value spec :kind nil))))
                (dependencies (dependencies spec)))
       ;; Multiple copy-artifact builders which copy artifacts from
       ;; other jobs.
       (iter (for dependency in dependencies)
             (let+ ((id      (as (value dependency :build-job-name) 'string))
                    (kind    (first (ensure-list (value dependency :kind nil))))
                    (pattern (when-let ((aspect (find-if (of-type 'aspect-archive-artifacts)
                                                         (aspects dependency))))
                               (as (value aspect :aspect.archive-artifacts.file-pattern nil)
                                   '(or null string))))
                    ((&flet matrix? (kind)
                       (member kind '("matrix" "matrix-project") :test #'string-equal)))
                    (reference (format nil "~A~@[/label=$label~]"
                                       id (matrix? kind))))
               (cond
                 ((not pattern)
                  (log:info "~@<Upstream project ~A does not provide ~
                             archived artifacts to copy into downstream ~
                             workspaces (variable ~S has no value).~@:>"
                            dependency :aspect.archive-artifacts.file-pattern))
                 ((and (matrix? kind) (not (matrix? self-kind)))
                  (error "~@<Upstream job ~A is of kind ~A, downstream ~
                          job ~A is of kind ~A.~@:>"
                         dependency kind job self-kind))
                 (t
                  (push (constraint! (build ((:after sloccount))
                                      copy-artifact)
                          (copy-artifact (:project-name reference
                                          :filter       pattern
                                          :target       upstream-dir
                                          :flatten?     t
                                          :clazz        "hudson.plugins.copyartifact.StatusBuildSelector")))
                        (builders job))
                  (setf copy-artifacts? t)))))

       ;; Shell builder which unpacks dependencies. Has to run after
       ;; artifact download, obviously.
       (when copy-artifacts?
         (push (constraint! (build ((:before cmake/unix)
                                    (:after copy-artifact)))
                 (shell (:command #?"cd ${upstream-dir}
find . -name '*.tar.gz' -exec tar -xzf '{}' \\;")))
               (builders job))))))

;;; Shell aspect

(define-aspect (shell :job-var job) (builder-defining-mixin)
    (((command (bail)) :type string
      :documentation
      "A fragment of shell code that should be executed as a build
       step at some point during the build."))
  "Adds a shell build step running COMMAND to the generated job.

   The ordering w.r.t. to other build steps is controlled via builder
   ordering constraints."
  (push (constraint! (build)
          (shell (:command (wrapped-shell-command (:aspect.shell) command))))
        (builders job)))

;;; Batch aspect

(define-aspect (batch :job-var job) (builder-defining-mixin)
    (((command (bail)) :type string
      :documentation
      "A fragment of Windows Batch doe that should be executed as a
       build step at some point during the build."))
  "Adds a Windows Batch build step running COMMAND to the generated job.

   The ordering w.r.t. to other build steps is controlled via builder
   ordering constraints."
  (push (constraint! (build) (batch (:command command)))
        (builders job)))

;;; CMake aspects

(define-aspect (cmake/unix :job-var  job
                           :spec-var spec)
    (builder-defining-mixin)
    ((command :type string
      :documentation
      "The shell command that should be executed to perform the CMake
       configuration and build."))
  "Configures a UNIX-specific CMake build step for the generated job.

   The build step executes the shell command in the COMMAND
   parameter.

   The value of this parameter can make use of the variables

     aspect.cmake/unix.find-commands
     aspect.cmake/unix.dir-options

   which contain shell fragments suitable for locating installed
   upstream CMake projects and passing this information to CMake via
   -DPROJECTNAME_Dir commandline options respectively.

   The ordering w.r.t. to other build steps is controlled via builder
   ordering constraints."
  (push (constraint! (build ((:after dependency-download)))
          (shell (:command (wrapped-shell-command (:aspect.cmake/unix) command))))
        (builders job)))

(let+ (((&flet shellify (name)
          (make-variable/sh (string-upcase name))))
       ((&flet map-cmake-requirements (function aspect)
          (let* ((project-version   (parent (parent aspect)))
                 (dependencies      (mapcar #'specification
                                            (list* project-version
                                                   (dependencies project-version))))
                 (seen-requirements (make-hash-table :test #'equal)))
            (iter outer (for dependency in dependencies)
                  (iter (for required in (uiop:symbol-call ; TODO hack
                                          '#:jenkins.model.project '#:requires-of-kind
                                          :cmake dependency))
                        (when-let ((provider (uiop:symbol-call ; TODO hack
                                              '#:jenkins.model.project '#:find-provider/version
                                              required :if-does-not-exist nil))
                                   (required (second required)))
                          (unless (gethash required seen-requirements)
                            (setf (gethash required seen-requirements) t)
                            (in outer (collect (funcall function required))))))))))
       ((&flet result (name raw)
          (values (cons name (value-parse raw)) '() t))))

  (defmethod lookup ((thing aspect-cmake/unix) (name (eql :aspect.cmake/unix.find-commands))
                     &key if-undefined)
    (declare (ignore if-undefined))
    (let+ (((&flet make-find (required)
              (format nil "~A_DIR=\"$(find \"${dependency-dir}\" ~
                                           -type f ~
                                           \\( ~
                                             -name \"~AConfig.cmake\" ~
                                             -o -name \"~:*~(~A~)-config.cmake\" ~
                                           \\) ~
                                           -exec dirname {} \\; ~
                                           -quit~
                                  )\"~%"
                      (shellify required) required))))
      (result name (map-cmake-requirements #'make-find thing))))

  (defmethod lookup ((thing aspect-cmake/unix) (name (eql :aspect.cmake/unix.dir-options))
                     &key if-undefined)
    (declare (ignore if-undefined))
    (let+ (((&flet make-option (required)
              (format nil "~A_DIR=\\${~A_DIR}"
                      required (shellify required)))))
      (result name (map-cmake-requirements #'make-option thing)))))

;;; Archive artifacts aspect

(define-aspect (cmake/win32 :job-var job) (builder-defining-mixin)
    ((command :type string
      :documentation
      "The batch command that should be executed to perform the CMake
       configuration and build."))
  "Configures a Win32-specific CMake build step for the generated job.

   The build step executes the shell command in the COMMAND
   parameter.

   Configuring upstream CMake projects is not yet supported.

   The ordering w.r.t. to other build steps is controlled via builder
   ordering constraints."
  (push (constraint! (((:after dependency-download)))
          (batch (:command command)))
        (builders job)))

;;; Archive artifact

(define-aspect (archive-artifacts :job-var job) (publisher-defining-mixin)
    (((file-pattern (bail)) :type (or null string)
      :documentation
      "An Ant-style file pattern designating the files that should be
       archived.

       See Jenkins documentation for details."))
  "Adds an artifact archiving publisher to the generated job.

   When multiple instances of this aspect are applied to a single job,
   the union of the respective FILE-PATTERNs is configured as the
   pattern of files to archive."
  (with-interface (publishers job) (archiver (publisher/archive-artifacts
                                              :files        nil
                                              :only-latest? nil))
    (when file-pattern
      (constraint! (publish) archiver) ; TODO slightly wrong
      (pushnew file-pattern (files archiver) :test #'string=))))

;;; Maven aspect

(define-aspect (maven :job-var job) (builder-defining-mixin)
    (((properties           '()) :type list #|of string|#
      :documentation
      "A list of Maven properties that should be set for the build.
       Entries are of the form NAME=VALUE.")
     (targets                    :type list #|of string|#
      :documentation
      "A list of names of Maven targets that should be built.")
     (private-repository?        :type boolean
      :documentation
      "See documentation of the Maven plugin.")
     ((settings-file        nil) :type (or null string)
      :documentation
      "Name of a Maven settings file that should be used for the Maven
       invocation.")
     ((global-settings-file nil) :type (or null string)
      :documentation
      "Name of a global Maven settings file that should be used for
       the Maven invocation."))
  "Configures a Maven build step for the generated job.

   The ordering w.r.t. to other build steps is controlled via builder
   ordering constraints."
  (push (constraint! (build)
         (maven (:properties          (mapcan (lambda (spec)
                                                (let+ (((name value) (split-option spec)))
                                                  (list (make-keyword name) value)))
                                              properties)
                                      ;; hack to prevent useless progress output
                                      ;; In the targets list because the maven
                                      ;; plugin does not have specific fields
                                      ;; for command line options
                 :targets             (list* "-B" targets)
                 :private-repository? private-repository?
                 :settings            (or settings-file :default)
                 :global-settings     (or global-settings-file :default))))
        (builders job)))

;;; Setuptools aspect

(define-aspect (setuptools :job-var job) (builder-defining-mixin)
    ((python-binary        :type string
      :documentation
      "Filename of the Python interpreter binary that should be
       invoked to perform the build.")
     (script               :type string
      :documentation
      "Name of the script (usually \"setup.py\") which should be
       executed to perform the setuptools-based configuration and
       build.")
     ((install-prefix nil) :type string
      :documentation
      "Prefix into which packages should be installed.

       Setting this variable to DIRECTORY will ensure the existence of
       DIRECTORY/lib/python2.7/site-packages or similar and place that
       directory on the PYTHONPATH.

       This variable does NOT affect the setuptools invocation. In
       particular, it does NOT imply python setup.py install
       --prefix=INSTALL-PREFIX or similar.")
     ((options        '()) :type list #|of string|#
      :documentation
      "A list of names of Setuptools option and corresponding values
       that should be set during the build. Entries are of the form

         [ \"SECTION\", \"NAME\", \"VALUE\" ]

       where SECTION is the usually the name of a command like
       \"install\", NAME is an option within the section such as
       \"prefix\" within the \"install\" section and VALUE is the
       desired value of the option.")
     (targets              :type list #|of string|#
      :documentation
      "A list of names of setuptools targets like \"install\" that
       should be built."))
  "Configures a Python setuptools build step for the generated job.

   The ordering w.r.t. to other build steps is controlled via builder
   ordering constraints."
  (let+ (;; Options
         ((&flet+ make-option ((section name value))
            #?"\${PYTHON} ${script} setopt -c ${section} -o ${name} -s \"${value}\""))
         (options (mapcar #'make-option options))
         ;; Targets
         ((&flet+ make-target ((name &optional no-fail?))
            #?"\${PYTHON} ${script} ${name} @{(when no-fail? '("|| true"))}"))
         (targets (mapcar (compose #'make-target #'ensure-list) targets))
         ;; Shell fragment that ensures existence of
         ;; {dist,site}-packages directory within install prefix.
         (ensure-install-directory
          (when install-prefix
            (format nil "INSTALL_DIRECTORY=\"$(~
                           ${PYTHON} -c ~
                           'from distutils.sysconfig import get_python_lib;~
                            print(get_python_lib(prefix=\"'~S'\"))'~
                         )\"~@
                         mkdir -p \"${INSTALL_DIRECTORY}\"~@
                         export PYTHONPATH=\"${PYTHONPATH}:${INSTALL_DIRECTORY}\""
                    install-prefix))))
    ;; Put everything into a shell fragment.
    (push (constraint! (build ((:after dependency-download)))
            (shell (:command (wrapped-shell-command (:aspect.setuptools)
                               (let ((interpol:*list-delimiter* #\newline))
                                 #?"PYTHON=${python-binary}

${(or ensure-install-directory "# Not creating install directory")}

# Configure options
@{(or options '("# No options configured"))}

# Process targets
@{(or targets '("# No targets configured"))}")))))
          (builders job))))

;;; Warnings aspect

(define-aspect (warnings :job-var job) (publisher-defining-mixin)
    ((parsers :type list #|of string|#
      :documentation
      "Names of parsers to apply to the output of the generated job.

       Parsers can be either builtin or defined in the global Jenkins
       configuration."))
  "Configures a warnings publisher for the generated job."
  (when parsers
    (with-interface (publishers job) (warnings (publisher/warnings))
      (iter (for parser in parsers)
            (pushnew (make-instance 'warning-parser/console :name parser)
                     (console-parsers warnings)
                     :test #'string=
                     :key  #'jenkins.api:name)))))

;;; Checkstyle and PMD aspects

(macrolet ((define (name publisher-name display-name)
             `(define-aspect (,name :job-var job) (publisher-defining-mixin)
                  ((pattern :type list
                    :documentation
                    "Analysis results should be read from files
                     matching the pattern."))
                ,(format nil "Configures a ~A publisher for the generated job."
                         display-name)
                (removef (publishers job) ',publisher-name :key #'type-of)
                (when pattern
                  (push (constraint! (publish)
                          (make-instance ',publisher-name :pattern pattern))
                        (publishers job))))))
  (define checkstyle publisher/checkstyle "CheckStyle")
  (define pmd        publisher/pmd        "PMD"))

;;; Test result aspects

(define-aspect (xunit :job-var job) (publisher-defining-mixin)
    ((kind                      :type string)
     (pattern                   :type (or null string))
     (skip-if-no-test-files?    :type boolean)
     (fail-if-not-new?          :type boolean)
     (delete-output-files?      :type boolean)
     (stop-processing-if-error? :type boolean))
  "Configures a publisher for XUnit test results for the generated job."
  (with-interface (publishers job) (publisher (publisher/xunit))
    (removef (types publisher) kind :test #'string= :key #'kind)
    (when pattern
      (push (make-instance
             'xunit/type
             :kind                      kind
             :pattern                   pattern
             :skip-if-no-test-files?    skip-if-no-test-files?
             :fail-if-not-new?          fail-if-not-new?
             :delete-output-files?      delete-output-files?
             :stop-processing-if-error? stop-processing-if-error?)
            (types publisher)))))

(define-aspect (junit :job-var job) (publisher-defining-mixin)
    ((pattern              :type (or null string)
      :documentation
      "Test results should be read from files matching the pattern.")
     (keep-long-stdio?     :type boolean
      :documentation
      "See plugin documentation.")
     (health-scale-factor  :type (or null positive-real)
      :documentation
      "Factor relating test failure counts to job health percentages.

       See plugin documentation for details.")
     (allow-empty-results? :type boolean
      :documentation
      "Controls whether empty test results should result in a build
       failure."))
  "Configures a publisher for JUnit test results for the generated job."
  ;; Remove previous configuration, if any.
  (removef (publishers job) 'publisher/junit :key #'type-of)
  ;; Add new configuration.
  (when pattern
    (push (constraint! (publish)
            (make-instance 'publisher/junit
                           :pattern              pattern
                           :keep-long-stdio?     keep-long-stdio?
                           :health-scale-factor  health-scale-factor
                           :allow-empty-results? allow-empty-results?))
          (publishers job))))

;;; Email notification

(define-aspect (email-notification :job-var job) (publisher-defining-mixin)
    ((recipients           :type list #|of string|#
      :documentation
      "A list of email addresses to which notifications in case of a
       build failure should be sent.")
     (send-to-perpetrator? :type boolean
      :documentation
      "Controls whether the authors of build-breaking commits are
       treated as additional recipients."))
  "Adds email notification in case of failed builds to a generated job."
  (removef (publishers job) 'publisher/email-notification :key #'type-of)
  (when recipients
    (push (constraint! (publish)
            (make-instance 'publisher/email-notification
                           :recipients           recipients
                           :send-to-individuals? send-to-perpetrator?))
          (publishers job))))

;;; Upload aspect

(define-aspect (upload :job-var job) (publisher-defining-mixin)
    ((target             :type string
      :documentation
      "The name of the machine to which artifacts should be uploaded.")
     (source-files       :type list #|of string|#
      :documentation
      "List of files to upload.")
     ((excludes     '()) :type list #|of string|#
      :documentation
      "Patterns for files which should be excluded from the upload.")
     (remove-prefix      :type string
      :documentation
      "A prefix string that should be removed from source filenames to
       obtain target filenames.")
     (remote-directory   :type string
      :documentation
      "Directory on the target machine into which artifacts should be
       uploaded.")
     (verbose?           :type boolean
      :documentation
      "Control the verbosity of the plugin during the upload process."))
  "Adds a publisher for uploads build results to the generated job."
  (push (constraint! (publish)
          (ssh (:target           target
                :source-files     source-files
                :excludes         excludes
                :remove-prefix    remove-prefix
                :remote-directory remote-directory
                :verbose?         verbose?)))
        (publishers job)))

;;; permissions aspect

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

;;; groovy script aspects

(define-aspect (groovy :job-var job) (builder-defining-mixin)
    (((code (bail)) :type string
      :documentation
      "The groovy code to execute in the build step."))
  "Configures a Groovy build step for the generated job.

   The ordering w.r.t. to other build steps is controlled via builder
   ordering constraints."
  (push (constraint! (build) (groovy (:code code))) (builders job)))

#.(interpol:disable-interpol-syntax)
