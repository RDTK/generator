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

(define-aspect (parameters) () ()
  (with-interface (properties job) (parameters (property/parameters))
    (mapc (lambda+ ((kind name &optional default))
            (setf (parameters parameters)
                  (remove name (parameters parameters)
                          :key (rcurry #'getf :name)))
            (let ((kind (cond
                          ((string= kind "text")   :text)
                          ((string= kind "string") :string)
                          (t
                           (error "~@<Unsupported parameter kind: ~S.~@:>"
                                  kind)))))
              (push (list* :kind kind :name name
                           (when default (list :default default)))
                    (parameters parameters))))
          (var/typed :aspect.parameters.parameters 'list))))

;;; Retention aspect

(define-aspect (retention :job-var job) () ()
  (setf (keep/days  job) (var/typed :aspect.retention.keep/days  '(or null positive-integer))
        (keep/count job) (var/typed :aspect.retention.keep/count '(or null positive-integer))))

;;; JDK aspect

(define-aspect (jdk) () ()
  (setf (jenkins.api::jdk job) (var/typed :aspect.jdk.jdk '(or null string) nil)))

;;; Github aspect

(define-aspect (github :job-var job) () ()
  (if-let ((project-url (var/typed :aspect.github.project-url '(or null string) nil)))
    (with-interface (properties job) (github (property/github))
      (setf (jenkins.api:project-url github) project-url
            (jenkins.api:display-name github)
            (var/typed :aspect.github.display-name '(or null string) nil)))
    (removef (properties job) 'property/github :key #'type-of)))

;;; Redmine aspects

(define-aspect (redmine) () ()
  (when-let* ((instance (var/typed :aspect.redmine.instance '(or null string) nil))
              (project  (var/typed :aspect.redmine.project  '(or null string) nil)))
    (setf (jenkins.api::redmine-instance job) instance
          (jenkins.api::redmine-project job)  project)))

(define-aspect (redmine-and-git
                :job-var     job
                :constraints ((:after aspect-git)))
    () ()
  (when-let* ((instance (var/typed :aspect.redmine.instance '(or null string) nil))
              (project  (var/typed :aspect.redmine.project  '(or null string) nil)))
    (let ((repository (repository job)))
      (unless (typep repository 'scm/git)
        (error "~@<Could not find git repository in ~A.~@:>" job))
      (setf (browser-kind repository) :redmine-web
            (browser-url  repository)
            (format nil "~A/projects/~A/repository/~@[~A/~]"
                    instance project
                    (var/typed :aspect.redmine.repository-id '(or null string) nil))))))

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

(defun slashify (namestring) ; TODO uiop:ensure-directory-pathname
  (if (ends-with #\/ namestring)
      namestring
      (concatenate 'string namestring "/")))

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
    `(let ((prefix (var/typed ,prefix-var-name '(or null string) nil))
           (suffix (var/typed ,suffix-var-name '(or null string) nil)))
       (wrap-shell-command (progn ,@body) prefix suffix))))

(defun split-option (spec)
  (let ((position (position #\= spec)))
    (unless position
      (error "~@<Option ~S is not of the form NAME=VALUE.~@:>"
             spec))
    (list (subseq spec 0 position) (subseq spec (1+ position)))))

(define-aspect (archive) (builder-defining-mixin)
    ()
  ;; In case we are updating an existing job, remove any repository
  ;; configuration.
  (setf (repository job) (make-instance 'scm/null))
  ;; Generate archive download and extraction as a shell builder.
  (let* ((url/string (var/typed :aspect.archive.url      'string))
         (url        (puri:uri url/string))
         (archive    (or (var/typed :aspect.archive.filename '(or null string) nil)
                         (lastcar (puri:uri-parsed-path url)))))
    (push (constraint! (((:before t)))
            (shell (:command #?"# Clean workspace.
${(make-remove-directory-contents/unix)}

# Unpack archive.
wget --no-verbose \"${url/string}\" --output-document=\"${archive}\"
unp -U \"${archive}\"
rm \"${archive}\"
directory=\$(find . -mindepth 1 -maxdepth 1)

${(make-move-stuff-upwards/unix '("${directory}"))}")))
          (builders job))))

(define-aspect (git :job-var job :aspect-var aspect) (builder-defining-mixin)
    ()
  ;; Configure GIT scm plugin.
  (let* ((url            (var/typed :aspect.git.url 'string))
         (url/parsed     (puri:uri url))
         (username       (var/typed :aspect.git.username '(or null string) nil))
         (password       (var/typed :aspect.git.password '(or null string) nil))
         (credentials    (or (var/typed :aspect.git.credentials '(or null string) nil)
                             (unless (check-access aspect :public)
                               (puri:uri-host url/parsed))))
         (branches       (var/typed :aspect.git.branches 'list))
         (local-branch   (var/typed :aspect.git.local-branch '(or null string) nil)))
    (setf (repository job)
          (git (:url                    (jenkins.analysis::format-git-url
                                         url/parsed username password)
                :credentials            credentials
                :branches               branches
                :clone-timeout          (var/typed :aspect.git.clone-timeout          '(or null integer) nil)
                :wipe-out-workspace?    (var/typed :aspect.git.wipe-out-workspace?    'boolean           nil)
                :clean-before-checkout? (var/typed :aspect.git.clean-before-checkout? 'boolean           nil)
                :checkout-submodules?   (var/typed :aspect.git.checkout-submodules?   'boolean           nil)
                :shallow?               (var/typed :aspect.git.shallow?               'boolean           nil)
                :local-branch           local-branch
                :internal-tag?          nil))))

  ;; If a specific sub-directory of the repository has been requested,
  ;; move the contents of that sub-directory to the top-level
  ;; workspace directory before proceeding.
  (when-let ((sub-directory (var/typed :sub-directory '(or null string) nil)))
    (let+ ((sub-directory (parse-namestring (slashify sub-directory)))
           ((&whole components first &rest &ign)
            (rest (pathname-directory sub-directory))))
      (push (constraint! (((:before t)))
              (shell (:command #?"${(make-remove-directory-contents/unix
                                     :exclude (list ".git" first))}

${(make-move-stuff-upwards/unix components)}")))
            (builders job)))))

(define-aspect (git-repository-browser
                :job-var     job
                :constraints ((:after aspect-git)))
    () ()
  (when-let* ((kind (var/typed :aspect.git-repository-browser.kind '(or null keyword) nil))
              (url  (var/typed :aspect.git-repository-browser.url  '(or null string)  nil)))
    (let ((repository (repository job)))
      (unless (typep repository 'scm/git)
        (error "~@<Could not find git repository in ~A.~@:>" job))
     (setf (browser-kind repository) kind
           (browser-url  repository) url))))

(define-aspect (subversion :job-var job :aspect-var aspect) () ()

  (let* ((url          (var/typed :aspect.subversion.url      'string))
         (revision     (var/typed :aspect.subversion.revision '(or null string) nil))
         (url/parsed   (puri:uri url))
         (url/parsed   (puri:copy-uri
                        url/parsed
                        :path (ppcre:regex-replace-all
                               "//+" (puri:uri-path url/parsed) "/")))
         (url/revision (format nil "~A~@[@~A~]" url/parsed revision))
         (credentials  (or (var/typed :aspect.subversion.credentials '(or null string) nil)
                           (unless (check-access aspect :public)
                             (puri:uri-host url/parsed)))))
    (setf (repository job)
          (svn (:url               url/revision
                :credentials       credentials
                :local-directory   (var/typed :aspect.subversion.local-dir '(or null string))
                :checkout-strategy (make-keyword
                                    (string-upcase
                                     (var/typed :aspect.subversion.checkout-strategy
                                                '(or string (eql :fresh-copy))
                                                :fresh-copy))))))))

(define-aspect (mercurial :job-var job :aspect-var aspect)
    (builder-defining-mixin)
    ()
  ;; Configure mercurial scm plugin.
  (let* ((url          (var/typed :aspect.mercurial.url 'string))
         (url/parsed   (puri:uri url))
         (credentials  (or (var/typed :aspect.mercurial.credentials '(or null string) nil)
                           (unless (check-access aspect :public)
                             (puri:uri-host url/parsed))))
         (branch       (var/typed :aspect.mercurial.branch '(or null string)))
         (tag          (var/typed :aspect.mercurial.tag    '(or null string)))
         (clean?       (var/typed :aspect.mercurial.clean? 'boolean          nil)))
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
  (when-let ((sub-directory (var/typed :sub-directory '(or null string) nil)))
    (let+ ((sub-directory (parse-namestring (slashify sub-directory)))
           ((&whole components first &rest &ign)
            (rest (pathname-directory sub-directory))))
      (push (constraint! (((:before t)))
              (shell (:command #?"${(make-remove-directory-contents/unix
                                     :exclude (list ".hg" first))}

${(make-move-stuff-upwards/unix components)}")))
            (builders job)))))

(define-aspect (trigger/scm) ()
    ()
  (removef (triggers job) 'trigger/scm :key #'type-of)
  (when-let ((trigger-spec (var/typed :aspect.trigger/scm.spec '(or null string))))
    (push (scm (:spec trigger-spec)) (triggers job))))

;;; Timeout aspect

(define-aspect (timeout) ()
    ()
  (when-let ((value (var/typed :aspect.timeout.timeout/minutes 'positive-integer)))
    (with-interface (build-wrappers job) (timeout (build-wrapper/timeout))
      (setf (timeout/minutes timeout) value))))

;;; Tasks aspect

(define-aspect (tasks) () ()
  (push (tasks (:pattern         (var/typed :aspect.tasks.pattern         'list)
                :exclude         (var/typed :aspect.tasks.exclude         'list)
                :keywords/low    (var/typed :aspect.tasks.keywords.low    'list)
                :keywords/normal (var/typed :aspect.tasks.keywords.normal 'list)
                :keywords/high   (var/typed :aspect.tasks.keywords.high   'list)))
        (publishers job)))

;;; SLOCcount aspect

(define-aspect (sloccount) (builder-defining-mixin)
    ()
  (let* ((directories (var/typed :aspect.sloccount.directories 'list))
         (arguments   (mapcar #'prin1-to-string directories)))
    (push (constraint! (((:before cmake/unix)
                         (:before maven)
                         (:before setuptools)))
            (shell (:command #?"DATA_DIR=\$(mktemp -d /tmp/build-generator.sloccount.data.XXXXXXXXXX)
REPORT_DIR=\$(mktemp -d /tmp/build-generator.sloccount.report.XXXXXXXXXX)
mkdir -p \"\${REPORT_DIR}\"
sloccount --datadir \"\${DATA_DIR}\" --wide --details @{arguments} > \"\${REPORT_DIR}/sloccount.sc\"
mv \"\${REPORT_DIR}/sloccount.sc\" \"\${WORKSPACE}/sloccount.sc\"
rm -rf \"\${DATA_DIR}\" \"\${REPORT_DIR}\"")))
         (builders job))

   (push (sloccount (:pattern "sloccount.sc"))
         (publishers job))))

;;; Slaves aspect

(define-aspect (slaves :job-var job) ()
    () ; TODO separate slaves aspect for matrix-project jobs?
  (when-let ((value (var/typed :aspect.slaves.slaves 'list '())))
    (setf (slaves job) value))
  (if-let ((value (var/typed :aspect.slaves.restrict-to-slaves '(or null string) nil)))
    (setf (can-roam? job)          nil
          (restrict-to-slaves job) value)
    (setf (can-roam? job) t)))

;;; Dependency download aspect

(define-aspect (dependency-download :job-var  job
                                    :spec-var spec)
    (builder-defining-mixin)
    ()
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
                 (push (constraint! (((:after sloccount))
                                     copy-artifact)
                         (copy-artifact (:project-name reference
                                         :filter       pattern
                                         :target       (var/typed :upstream-dir 'string)
                                         :flatten?     t
                                         :clazz        "hudson.plugins.copyartifact.StatusBuildSelector")))
                       (builders job))
                 (setf copy-artifacts? t)))))

      ;; Shell builder which unpacks dependencies. Has to run after
      ;; artifact down, obviously.
      (when copy-artifacts?
        (push (constraint! (((:before cmake/unix)
                             (:after copy-artifact)))
                (shell (:command #?"cd ${(var/typed :upstream-dir 'string)}
find . -name '*.tar.gz' -exec tar -xzf '{}' \\;")))
              (builders job))))))

;;; Shell aspect

(define-aspect (shell :job-var job) (builder-defining-mixin)
    ()
  (when-let ((command (var/typed :aspect.shell.command '(or null string))))
    (push (constraint! ()
            (shell (:command (wrapped-shell-command (:aspect.shell) command))))
          (builders job))))

;;; Batch aspect

(define-aspect (batch :job-var job) (builder-defining-mixin)
    ()
  (when-let ((command (var/typed :aspect.batch.command '(or null string))))
    (push (constraint! () (batch (:command command)))
          (builders job))))

;;; CMake aspects

(define-aspect (cmake/unix :job-var  job
                           :spec-var spec)
    (builder-defining-mixin)
    ()
  (push (constraint! (((:after dependency-download)))
          (shell (:command (wrapped-shell-command (:aspect.cmake/unix)
                             (var/typed :aspect.cmake/unix.command 'string)))))
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

(define-aspect (cmake/win32 :job-var job)
    (builder-defining-mixin)
    ()
  (push (constraint! (((:after dependency-download)))
          (batch (:command (var/typed :aspect.cmake/win32.command 'string))))
        (builders job)))

;;; Archive artifact

(define-aspect (archive-artifacts :job-var job) ()
    ()
  (when-let ((file-pattern (var/typed :aspect.archive-artifacts.file-pattern '(or null string) nil)))
    (with-interface (publishers job) (archiver (publisher/archive-artifacts
                                                :files        nil
                                                :only-latest? nil))
      (pushnew file-pattern (files archiver) :test #'string=))))

;;; Maven aspect

(define-aspect (maven :job-var job) (builder-defining-mixin)
    ()
  (push (constraint! ()
         (maven (:properties          (mapcan (lambda (spec)
                                                (let+ (((name value) (split-option spec)))
                                                  (list (make-keyword name) value)))
                                              (var/typed :aspect.maven.properties 'list))
                                      ;; hack to prevent useless progress output
                                      ;; In the targets list because the maven
                                      ;; plugin does not have specific fields
                                      ;; for command line options
                 :targets             (list* "-B" (var/typed :aspect.maven.targets 'list))
                 :private-repository? (var/typed :aspect.maven.private-repository? 'boolean)
                 :settings            (or (var/typed :aspect.maven.settings-file        '(or null string) nil) :default)
                 :global-settings     (or (var/typed :aspect.maven.global-settings-file '(or null string) nil) :default))))
        (builders job)))

;;; Setuptools aspect

(define-aspect (setuptools :job-var job) (builder-defining-mixin)
    ()
  (let+ ((binary         (var/typed :aspect.setuptools.python-binary  'string))
         (script         (var/typed :aspect.setuptools.script         'string))
         (install-prefix (var/typed :aspect.setuptools.install-prefix '(or null string) nil))
         ;; Options
         ((&flet+ make-option ((section name value))
            #?"\${PYTHON} ${script} setopt -c ${section} -o ${name} -s \"${value}\""))
         (options (mapcar #'make-option (var/typed :aspect.setuptools.options 'list '())))
         ;; Targets
         ((&flet+ make-target ((name &optional no-fail?))
            #?"\${PYTHON} ${script} ${name} @{(when no-fail? '("|| true"))}"))
         (targets (mapcar (compose #'make-target #'ensure-list)
                          (var/typed :aspect.setuptools.targets 'list)))
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
    (push (constraint! (((:after dependency-download)))
            (shell (:command (wrapped-shell-command (:aspect.setuptools)
                               (let ((interpol:*list-delimiter* #\newline))
                                 #?"PYTHON=${binary}

${(or ensure-install-directory "# Not creating install directory")}

# Configure options
@{(or options '("# No options configured"))}

# Process targets
@{(or targets '("# No targets configured"))}")))))
          (builders job))))

;;; Warnings aspect

(define-aspect (warnings :job-var job) ()
    ()
  (when-let ((parsers (var/typed :aspect.warnings.parsers 'list)))
    (with-interface (publishers job) (warnings (publisher/warnings))
      (iter (for parser in parsers)
            (pushnew (make-instance 'warning-parser/console :name parser)
                     (console-parsers warnings)
                     :test #'string=
                     :key  #'jenkins.api:name)))))

;;; Checkstyle and PMD aspects

(macrolet ((define (name publisher-name)
             (let ((variable-name (let ((*package* (find-package '#:keyword)))
                                    (symbolicate '#:aspect. name '#:.pattern))))
               `(define-aspect (,name :job-var job) ()
                    ()
                  (removef (publishers job) ',publisher-name :key #'type-of)
                  (when-let ((pattern (var/typed ,variable-name 'list)))
                    (appendf (publishers job)
                             (list (make-instance ',publisher-name
                                                  :pattern pattern))))))))
  (define checkstyle publisher/checkstyle)
  (define pmd        publisher/pmd))

;;; Test result aspects

(define-aspect (xunit :job-var job) ()
    ()
  (let ((kind (var/typed :aspect.xunit.kind 'string)))
    (with-interface (publishers job) (publisher (publisher/xunit))
      (removef (types publisher) kind :test #'string= :key #'kind)
      (push (make-instance
             'xunit/type
             :kind                      kind
             :pattern                   (var/typed :aspect.xunit.pattern                   'string)
             :skip-if-no-test-files?    (var/typed :aspect.xunit.skip-if-no-test-files?    'boolean)
             :fail-if-not-new?          (var/typed :aspect.xunit.fail-if-not-new?          'boolean)
             :delete-output-files?      (var/typed :aspect.xunit.delete-output-files?      'boolean)
             :stop-processing-if-error? (var/typed :aspect.xunit.stop-processing-if-error? 'boolean))
            (types publisher)))))

(define-aspect (junit :job-var job) ()
    ()
  (removef (publishers job) 'publisher/junit :key #'type-of)
  (when-let ((pattern (var/typed :aspect.junit.pattern '(or null string))))
    (let ((keep-long-stdio?     (var/typed :aspect.junit.keep-long-stdio?     'boolean))
          (health-scale-factor  (var/typed :aspect.junit.health-scale-factor  '(or null positive-real)))
          (allow-empty-results? (var/typed :aspect.junit.allow-empty-results? 'boolean)))
      (appendf (publishers job)
               (list (make-instance 'publisher/junit
                                    :pattern              pattern
                                    :keep-long-stdio?     keep-long-stdio?
                                    :health-scale-factor  health-scale-factor
                                    :allow-empty-results? allow-empty-results?))))))

;;; Email notification

(define-aspect (email-notification :job-var job) ()
    ()
  (if-let ((recipients (var/typed :aspect.email-notification.recipients 'list)))
    (let ((send-to-perpetrator? (var/typed :aspect.email-notification.send-to-perpetrator? 'boolean)))
      (with-interface (publishers job)
          (publisher (publisher/email-notification
                      :recipients           recipients
                      :send-to-individuals? send-to-perpetrator?))
        (declare (ignore publisher))))
    (removef (publishers job) 'publisher/email-notification :key #'type-of)))

;;; Upload aspect

(define-aspect (upload :job-var job) ()
    ()
  (push (ssh (:target           (var/typed :aspect.upload.target        'string)
              :source-files     (var/typed :aspect.upload.source-files  'list)
              :excludes         (var/typed :aspect.upload.excludes      'list   '())
              :remove-prefix    (var/typed :aspect.upload.remove-prefix 'string)
              :remote-directory (var/typed :aspect.upload.dir           'string)
              :verbose?         nil))
        (publishers job)))

;;; permissions aspect

(define-aspect (permissions :job-var job) ()
    ()
  (let+ (((&flet+ normalize-permission ((subject action))
            (list subject (mapcar (compose #'make-keyword #'string-upcase)
                                  action))))
         (new-permissions (var/typed :aspect.permissions.permissions '(or (eql :keep) list) :keep)))
    (unless (eq new-permissions :keep)
      (setf (permissions job) (mapcar #'normalize-permission new-permissions)))))

;;; groovy script aspects

(define-aspect (groovy :job-var job) (builder-defining-mixin)
    ()
  (when-let ((code (var/typed :aspect.groovy.code '(or null string))))
    (push (constraint! () (groovy (:code code))) (builders job))))

#.(interpol:disable-interpol-syntax)
