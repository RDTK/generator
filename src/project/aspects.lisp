;;;; aspects.lisp ---
;;;;
;;;; Copyright (C) 2012, 2013, 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.project)

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
          (var :aspect.parameters.parameters))))

;;; Retention aspect

(define-aspect (retention :job-var job) () ()
  (setf (keep/days  job) (var :aspect.retention.keep/days)
        (keep/count job) (var :aspect.retention.keep/count)))

;;; JDK aspect

(define-aspect (jdk) () ()
  (setf (jenkins.api::jdk job) (var :aspect.jdk.jdk nil)))

;;; Redmine aspect

(define-aspect (redmine) () ()
  (when-let* ((instance (var :aspect.redmine.instance nil))
              (project  (var :aspect.redmine.project nil)))
    (setf (jenkins.api::redmine-instance job) (format nil "~A/" instance)
          (jenkins.api::redmine-project job) project)
    (when-let ((version (var :aspect.redmine.version nil)))
      (setf (jenkins.api::redmine-version job) version))))

(define-aspect (redmine-and-git
                :job-var     job
                :constraints ((:after aspect-git)))
    () ()
  (when-let* ((instance (var :aspect.redmine.instance nil))
              (project  (var :aspect.redmine.project nil)))
    (let ((repository (repository job)))
      (unless (typep repository 'scm/git)
        (error "~@<Could not find git repository in ~A.~@:>" job))
      (setf (browser-kind repository) :redmine-web
            (browser-url  repository)
            (format nil "~A/projects/~A/repository/~@[~A/~]"
                    instance project
                    (var :aspect.redmine.repository-id nil))))))

;;; SCM aspects

(defun make-remove-directory-contents (&key exclude)
  (let ((exclude (ensure-list exclude)))
    (format nil "find . -mindepth 1 -maxdepth 1 ~
                        ~[~:*~;-not -name ~{~S~} ~:;-not \\( ~{-name ~S~^ -o ~} \\) ~]~
                        -exec rm -rf {} \\;"
            (length exclude) exclude)))
(assert
 (string= (make-remove-directory-contents)
          "find . -mindepth 1 -maxdepth 1 -exec rm -rf {} \\;"))
(assert
 (string= (make-remove-directory-contents :exclude "foo")
          "find . -mindepth 1 -maxdepth 1 -not -name \"foo\" -exec rm -rf {} \\;"))
(assert
 (string= (make-remove-directory-contents :exclude '("b\"ar" "foo"))
          "find . -mindepth 1 -maxdepth 1 -not \\( -name \"b\\\"ar\" -o -name \"foo\" \\) -exec rm -rf {} \\;"))

(defun make-move-stuff-upwards/unix (stuff)
  "Move contents of STUFF which is usually one or multiple directories
   to the current directory."
  (declare (type list stuff))
  (let+ (((first &rest rest) stuff)
         (rest/string (namestring (make-pathname :directory `(:relative ,@rest)))))
    #?"# Uniquely rename directory.
temp=\$(mktemp -d ./XXXXXXXX)
mv -T \"${first}\" \"\${temp}/\"

# Move contents to toplevel workspace directory.
find \"\${temp}/${rest/string}\" -mindepth 1 -maxdepth 1 -exec mv {} . \\;
rm -rf \"\${temp}\""))

(defun slashify (namestring)
  (if (ends-with #\/ namestring)
      namestring
      (concatenate 'string namestring "/")))

(defun make-variable/sh (string)
  (substitute-if #\_ (lambda (character)
                       (not (or (alphanumericp character)
                                (member character '(#\_)))))
                 string))

(define-aspect (archive) (builder-defining-mixin)
    ()
  (let* ((url/string (var :aspect.archive.url))
         (url        (puri:uri url/string))
         (archive    (var :aspect.archive.filename
                          (lastcar (puri:uri-parsed-path url)))))
    (push (constraint! (((:before t)))
            (shell (:command #?"# Clean workspace.
${(make-remove-directory-contents)}

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
  (let* ((url          (var :aspect.git.url))
         (url/parsed   (puri:uri url))
         (username     (var :aspect.git.username nil))
         (password     (var :aspect.git.password nil))
         (credentials  (or (var :aspect.git.credentials nil)
                           (unless (check-access aspect :public)
                             (puri:uri-host url/parsed))))
         (branches     (var :aspect.git.branches))
         (local-branch (or (var :aspect.git.local-branch nil)
                           (first branches))))
    (setf (repository job)
          (git (:url                  (jenkins.analysis::format-git-url
                                       url/parsed username password)
                :credentials          credentials
                :branches             branches
                :wipe-out-workspace?  (var :aspect.git.wipe-out-workspace? t)
                :checkout-submodules? (var :aspect.git.checkout-submodules? nil)
                :shallow?             (var :aspect.git.shallow? nil)
                :local-branch         local-branch
                :internal-tag?        nil))))

  ;; If a specific sub-directory of the repository has been requested,
  ;; move the contents of that sub-directory to the top-level
  ;; workspace directory before proceeding.
  (when-let ((sub-directory (var :sub-directory nil)))
    (let+ ((sub-directory (parse-namestring (slashify sub-directory)))
           ((&whole components first &rest &ign)
            (rest (pathname-directory sub-directory))))
      (push (constraint! (((:before t)))
              (shell (:command #?"${(make-remove-directory-contents :exclude first)}

${(make-move-stuff-upwards/unix components)}")))
            (builders job)))))

(define-aspect (subversion :job-var job :aspect-var aspect) () ()
  (let* ((url         (var :aspect.subversion.url))
         (revision     (var :aspect.subversion.revision nil))
         (url/parsed  (puri:uri url))
         (url/parsed  (puri:copy-uri
                       url/parsed
                       :path (ppcre:regex-replace-all
                              "//+" (puri:uri-path url/parsed) "/")))
         (url/revision (format nil "~A~[@~A~]" url/parsed revision))
         (credentials (or (var :aspect.subversion.credentials)
                          (unless (check-access aspect :public)
                            (puri:uri-host url/parsed)))))
    (setf (repository job)
          (svn (:url               url/revision
                :credentials       credentials
                :local-directory   (var :aspect.subversion.local-dir)
                :checkout-strategy (make-keyword
                                    (string-upcase
                                     (var :aspect.subversion.checkout-strategy
                                          :fresh-copy))))))))

(define-aspect (mercurial :job-var job :aspect-var aspect)
    (builder-defining-mixin)
    ()
  ;; Configure mercurial scm plugin.
  (let* ((url          (var :aspect.mercurial.url))
         (url/parsed   (puri:uri url))
         (credentials  (or (var :aspect.mercurial.credentials nil)
                           (unless (check-access aspect :public)
                             (puri:uri-host url/parsed))))
         (branch       (var :aspect.mercurial.branch))
         (tag          (var :aspect.mercurial.tag))
         (clean?       (var :aspect.mercurial.clean?)))
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
  (when-let ((sub-directory (var :sub-directory nil)))
    (let+ ((sub-directory (parse-namestring (slashify sub-directory)))
           ((&whole components first &rest &ign)
            (rest (pathname-directory sub-directory))))
      (push (constraint! (((:before t)))
              (shell (:command #?"${(make-remove-directory-contents :exclude first)}

${(make-move-stuff-upwards/unix components)}")))
            (builders job)))))

(define-aspect (trigger/scm) ()
    ()
  (push (scm (:spec (var :aspect.trigger/scm.spec)))
        (triggers job)))

;;; Timeout aspect

(define-aspect (timeout)
    ()
    ()
  (when-let ((value (var :aspect.timeout.timeout/minutes)))
    (with-interface (build-wrappers job) (timeout (build-wrapper/timeout))
      (setf (timeout/minutes timeout) value))))

;;; Tasks aspect

(define-aspect (tasks) () ()
  (push (tasks (:pattern         (var :aspect.tasks.pattern)
                :exclude         (var :aspect.tasks.exclude)
                :keywords/low    (var :aspect.tasks.keywords.low)
                :keywords/normal (var :aspect.tasks.keywords.normal)
                :keywords/high   (var :aspect.tasks.keywords.high)))
        (publishers job)))

;;; builder-defining-aspect-mixin

(defclass aspect-builder-defining-mixin ()
  ()
  (:documentation
   "TODO"))

(defun+ parse-constraint ((&whole raw kind subject))
  (let+ (((&flet parse-kind ()
            (make-keyword (string-upcase kind))))
         ((&flet parse-class-or-tag (value)
            (if (or (equal value "<all>") (eq value t))
                t
                (intern (string-upcase value) #.*package*))))
         ((&flet parse-name (value)
            (if (or (equal value "<all>") (eq value t))
                t
                value))))
    (cond
      ((equal subject "<all>")
       (list (parse-kind) t t))
      ((stringp subject)
       (list (parse-kind) (parse-class-or-tag subject) t))
      ((consp subject)
       (let+ (((&plist-r/o (class-or-tag :type t) (name :name t))
               (alist-plist subject)))
         (list (parse-kind)
               (parse-class-or-tag class-or-tag)
               (parse-name name))))
      (t
       (error 'type-error
              :datum         raw
              :expected-type '(or (eql "<all>") cons))))))

(mapc (lambda+ ((json expected))
        (assert (equal expected (parse-constraint
                                 (json:decode-json-from-string json)))))
      '(("[ \"before\", \"<all>\"               ]"                  (:before t t))
        ("[ \"before\", \"foo\"                 ]"                  (:before foo t))
        ("[ \"before\", { \"type\": \"foo\" }   ]"                  (:before foo t))
        ("[ \"before\", { \"type\": \"<all>\" } ]"                  (:before t t))
        ("[ \"before\", { \"name\": \"bar\" }   ]"                  (:before t "bar"))
        ("[ \"before\", { \"name\": \"<all>\" } ]"                  (:before t t))
        ("[ \"before\", { \"type\": \"fez\", \"name\": \"baz\" } ]" (:before fez "baz"))))

(defmethod builder-constraints ((aspect  aspect-builder-defining-mixin)
                                (builder t))
  (let+ ((builder-type    (type-of builder))
         (variable        (format-symbol
                           :keyword "ASPECT.BUILDER-CONSTRAINTS.~@:(~A~)"
                           (let ((type-string (string builder-type)))
                             (subseq type-string (length "builder/")))))
         (constraints/raw (value aspect variable nil))
         (constraints     (mapcar #'parse-constraint constraints/raw)))
    (log:trace "Constraints for ~A in ~A: ~S" builder variable constraints)
    constraints))

;;; SLOCcount aspect

(define-aspect (sloccount) (builder-defining-mixin) ()
  (push (constraint! (((:before cmake/unix)))
         (shell (:command "TEMPDIR=$(mktemp -d /tmp/tmp.XXXXXXXXXX)
sloccount --datadir \"${TEMPDIR}\" --wide --details \"${WORKSPACE}\" > \"${WORKSPACE}/sloccount.sc\"
rm -rf \"${TEMPDIR}\"")))
        (builders job))

  (push (sloccount (:pattern "sloccount.sc"))
        (publishers job)))

;;; Slaves aspect

(define-aspect (slaves :job-var job) () () ; TODO separate slaves aspect for matrix-project jobs?
  (when-let ((value (var :aspect.slaves.slaves nil)))
    (setf (slaves job) value))
  (if-let ((value (var :aspect.slaves.restrict-to-slaves nil)))
    (setf (can-roam? job)          nil
          (restrict-to-slaves job) value)
    (setf (can-roam? job) t)))

;;; Dependency download aspect

(define-aspect (dependency-download :job-var  job
                                    :spec-var spec)
    (builder-defining-mixin) ()
  (let ((copy-artifacts? nil))
    (when-let ((self-kind    (first (ensure-list (ignore-errors
                                                  (value spec :kind)))))
               (dependencies (dependencies spec)))
      ;; Multiple copy-artifact builders which copy artifacts from
      ;; other jobs.
      (iter (for dependency in dependencies)
            (let+ ((id      (value dependency :build-job-name))
                   (kind    (first (ensure-list (ignore-errors
                                                 (value dependency :kind)))))
                   (pattern (when-let ((aspect (find-if (of-type 'aspect-archive-artifacts)
                                                        (aspects dependency))))
                              (ignore-errors
                               (value aspect :aspect.archive-artifacts.file-pattern))))
                   ((&flet matrix? (kind)
                      (member kind '("matrix" "matrix-project") :test #'string-equal)))
                   (reference (format nil "~A~@[/label=$label~]"
                                      id (matrix? kind))))
              (cond
                ((not pattern)
                 (log:info "~@<Upstream project ~A does not provide ~
                            archived artifacts to copy into downstream ~
                            workspaces (variable ~S has no ~
                            value).~@:>"
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
                                         :target       (var :upstream-dir)
                                         :flatten?     t
                                         :clazz        "hudson.plugins.copyartifact.StatusBuildSelector")))
                       (builders job))
                 (setf copy-artifacts? t)))))

      ;; Shell builder which unpacks dependencies. Has to run after
      ;; artifact down, obviously.
      (when copy-artifacts?
        (push (constraint! (((:before cmake/unix)
                             (:after copy-artifact)))
                (shell (:command #?"cd ${(var :upstream-dir)}
for archive in *.tar.gz ; do tar -xzf \"\${archive}\" ; done")))
              (builders job))))))

; dependency-download/windows
#|
cd upstream
unzip -o *.zip
move *.zip ..
move RSC* RSC
move RSBProtocol* RSBProtocol
move ..\*.zip .
|#

;;; Shell aspect

(define-aspect (shell :job-var job) (builder-defining-mixin) ()
  (when-let ((command (var :aspect.shell.command)))
    (push (constraint! () (shell (:command command)))
          (builders job))))

;;; CMake aspects

(define-aspect (cmake/unix :job-var  job
                           :spec-var spec)
    (builder-defining-mixin) ()
  (let+ (((&flet shellify (name)
           (make-variable/sh (string-upcase name))))
         (variables (iter (for variable in (var :aspect.cmake.environment '())) ; TODO check these for validity?
                          (collect (format nil "export ~A~%" variable))))
         ((&flet+ format-option ((name value))
            (format nil "-D~A=~A \\~%" name value)))
         (project-version (parent spec))
         (dependencies    (mapcar #'specification (list* project-version
                                                         (dependencies project-version))))
         ((&values finds dir-options/raw)
          (iter outer (for dependency in dependencies)
                (iter (for (_ required) in (requires-of-kind
                                            :cmake dependency))
                      (in outer
                          (collect #?"${(shellify required)}_DIR=\"\$(find \"${(var :dependency-dir)}\" -type f \\( -name \"${required}Config.cmake\" -o -name \"${(string-downcase required)}-config.cmake\" \\) -exec dirname {} \\;)\"\n"
                            :into finds)
                          (collect (list #?"${required}_DIR"
                                         #?"\${${(shellify required)}_DIR}")
                            :into options)))
                (finally (return-from outer (values finds options)))) )
         (options/raw (append dir-options/raw
                              (mapcar (curry #'split-sequence #\=)
                                      (var :aspect.cmake.options '()))))
         (options     (mapcar #'format-option options/raw))
         (targets     (var :aspect.cmake.targets '())))

    (push (constraint! (((:after dependency-download)))
            (shell (:command #?"mkdir -p ${(var :build-dir)} && cd ${(var :build-dir)}

@{variables}

@{finds}
cmake @{options} ..
make # not always necessary, but sometimes, sadly
make @{targets}")))
          (builders job))))

(define-aspect (archive-artifacts :job-var job) () ()
  (when-let ((file-pattern (var :aspect.archive-artifacts.file-pattern nil)))
    (with-interface (publishers job) (archiver (publisher/archive-artifacts
                                                :files        nil
                                                :only-latest? nil))
      (pushnew file-pattern (files archiver) :test #'string=))))

(define-aspect (cmake/windows) (builder-defining-mixin) ()
  (push (constraint! ()
         (batch (:command "setlocal EnableDelayedExpansion

SET COMMON_ROOT=VS%VS_VERSION%COMNTOOLS
call \"!%COMMON_ROOT%!/vsvars32.bat\"

ECHO %MSVC100_VOL%
SET VOL_VAR=MSVC%VS_VERSION%_VOL

SET /A TEST_PORT=5000+%VS_VERSION%

call project\build_vs.bat -DCMAKE_BUILD_TYPE=debug -DPROTOBUF_ROOT=\"!%VOL_VAR%!\protobuf\" \"-DRSC_DIR=%WORKSPACE%\upstream\RSC\share\rsc0.9\" \"-DRSBProtocol_DIR=%WORKSPACE%\upstream\RSBProtocol\share\rsbprotocol\" -DSPREAD_ROOT=!%VOL_VAR%!\spread -DTEST_SPREAD_PORT=%TEST_PORT%")))
        (builders job)))

;;; Maven aspect

(define-aspect (maven :job-var job) (builder-defining-mixin) ()
  (push (constraint! ()
         (maven (:properties          (mapcan (lambda (spec)
                                                (let+ (((name value) (split-sequence #\= spec)))
                                                  (list (make-keyword name) value)))
                                              (var :aspect.maven.properties))
                                      ;; hack to prevent useless progress output
                                      ;; In the targets list because the maven
                                      ;; plugin does not have specific fields
                                      ;; for command line options
                 :targets             (list* "-B" (var :aspect.maven.targets))
                 :private-repository? (var :aspect.maven.private-repository?)
                 :settings            (var :aspect.maven.settings-file :default)
                 :global-settings     (var :aspect.maven.global-settings-file :default))))
        (builders job)))

;;; Setuptools aspect

(define-aspect (setuptools :job-var job) (builder-defining-mixin) ()
  (let+ ((binary         (var :aspect.setuptools.python-binary))
         (script         (var :aspect.setuptools.script))
         (install-prefix (var :aspect.setuptools.install-prefix nil))
         ;; Options
         ((&flet+ make-option ((section name value))
            #?"\${PYTHON} ${script} setopt -c ${section} -o ${name} -s \"${value}\""))
         (options (mapcar #'make-option (var :aspect.setuptools.options '())))
         ;; Targets
         ((&flet+ make-target ((name &optional no-fail?))
            #?"\${PYTHON} ${script} ${name} @{(when no-fail? '("|| true"))}"))
         (targets (mapcar (compose #'make-target #'ensure-list)
                          (var :aspect.setuptools.targets)))
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
            (shell (:command (let ((interpol:*list-delimiter* #\newline))
                               #?"PYTHON=${binary}

${(or ensure-install-directory "# Not creating install directory")}

# Configure options
@{(or options '("# No options configured"))}

# Process targets
@{(or targets '("# No targets configured"))}"))))
          (builders job))))

;;; Warnings aspect

(define-aspect (warnings :job-var job) () ()
  (with-interface (publishers job) (warnings (publisher/warnings))
    (iter (for parser in (var :warning-parsers))
          (pushnew (make-instance 'warning-parser/console :name parser)
                   (console-parsers warnings)
                   :test #'string=
                   :key  #'name))))

;;; Test result aspects

(define-aspect (xunit :job-var job) () ()
  (let ((kind (var :aspect.xunit.kind)))
    (with-interface (publishers job) (publisher (publisher/xunit))
      (removef (types publisher) kind :test #'string= :key #'kind)
      (push (make-instance
             'xunit/type
             :kind                      kind
             :pattern                   (var :aspect.xunit.pattern)
             :skip-if-no-test-files?    (var :aspect.xunit.skip-if-no-test-files?)
             :fail-if-not-new?          (var :aspect.xunit.fail-if-not-new?)
             :delete-output-files?      (var :aspect.xunit.delete-output-files?)
             :stop-processing-if-error? (var :aspect.xunit.stop-processing-if-error?))
            (types publisher)))))

(define-aspect (junit :job-var job) () ()
  ;; Remove previous configuration, if any.
  (removef (publishers job) 'publisher/junit :key #'of-type)
  ;; Add new configuration.
  (appendf (publishers job)
           (list (make-instance
                  'publisher/junit
                  :pattern             (var :aspect.junit.pattern)
                  :keep-long-stdio?    (var :aspect.junit.keep-long-stdio?)
                  :health-scale-factor (var :aspect.junit.health-scale-factor)))))

;;; Debian packaging aspects

(define-aspect (debian-package :job-var job) () ()
  ;; Add console-based parser for lintian.
  (with-interface (publishers job) (warnings (publisher/warnings))
    (pushnew (make-instance 'warning-parser/console :name "Lintian")
             (console-parsers warnings)
             :test #'string=
             :key  #'name))

  ;; Archive the generated Debian package.
  (with-interface (publishers job) (archiver (publisher/archive-artifacts
                                              :files        nil
                                              :only-latest? nil))
    (pushnew #?"${(var :build-dir)}/*.deb" (files archiver)
             :test #'string=)))

(define-aspect (debian-package/cmake) (debian-package
                                       builder-defining-mixin)
    ()
  ;; TODO add PACKAGE_REVISION to environment
  (push (constraint! (((:after cmake/unix)))
          (shell (:command #?"mkdir -p ${(var :build-dir)} && cd ${(var :build-dir)}
cmake -DCPACK_CONFIG_FILE=${(var :aspect.debian-package/cmake.cpack-config-file)} \\
      -DCPACK_PACKAGE_REVISION=\${PACKAGE_REVISION} \\
      ..
umask 022
\${FAKEROOT_FOR_CPACK} make package
lintian -i *.deb || true
")))
        (builders job)))

;;; upload aspect

(define-aspect (upload :job-var job) () ()
  (push (ssh (:target           (var :aspect.upload.target)
              :source-files     (var :aspect.upload.source-files)
              :excludes         (var :aspect.upload.excludes '())
              :remove-prefix    (var :aspect.upload.remove-prefix)
              :remote-directory (var :aspect.upload.dir)
              :verbose?         nil))
        (publishers job)))

#.(interpol:disable-interpol-syntax)
