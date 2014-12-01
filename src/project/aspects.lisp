;;;; aspects.lisp ---
;;;;
;;;; Copyright (C) 2012, 2013, 2014 Jan Moringen
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

(define-aspect (retention) () ()
  (setf (keep/days  job) (var :keep/days)
        (keep/count job) (var :keep/count)))

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

(define-aspect (archive)
    ()
    ()
  (let* ((url/string (var :aspect.archive.url))
         (url        (puri:uri url/string))
         (archive    (var :aspect.archive.filename
                          (lastcar (puri:uri-parsed-path url)))))
    (push (constraint! (((:before t)))
            (shell (:command #?"# Clean workspace.
rm -rf *

# Unpack archive.
wget --no-verbose \"${url/string}\" --output-document=\"${archive}\"
unp -U \"${archive}\"
rm \"${archive}\"
directory=\$(find . -mindepth 1 -maxdepth 1)

${(make-move-stuff-upwards/unix '("${directory}"))}")))
          (builders job))))

(define-aspect (git)
    ()
    ()
  ;; Configure GIT scm plugin.
  (setf (repository job)
        (git (:url                  (jenkins.analysis::format-git-url
                                     (puri:uri (var :aspect.git.url))
                                     (var :aspect.git.username nil)
                                     (var :aspect.git.password nil))
              :branches             (var :aspect.git.branches)
              :wipe-out-workspace?  (var :aspect.git.wipe-out-workspace? t)
              :checkout-submodules? (var :aspect.git.checkout-submodules? nil)
              :shallow?             (var :aspect.git.shallow? nil)
              :local-branch         (first (var :aspect.git.branches))
              :internal-tag?        nil)))

  ;; If a specific sub-directory of the repository has been requested,
  ;; move the contents of that sub-directory to the top-level
  ;; workspace directory before proceeding.
  (when-let ((sub-directory (var :sub-directory nil)))
    (let+ ((sub-directory (parse-namestring (slashify sub-directory)))
           ((&whole components first &rest &ign)
            (rest (pathname-directory sub-directory))))
      (push (constraint! (((:before t)))
              (shell (:command #?"find . -mindepth 1 -maxdepth 1 -not -name \"${first}\" -exec rm -rf {} \\;

${(make-move-stuff-upwards/unix components)}")))
            (builders job)))))

(define-aspect (subversion) () ()
  (setf (repository job)
        (svn (:url               (var :aspect.subversion.url)
              :local-directory   (var :aspect.subversion.local-dir)
              :checkout-strategy (make-keyword
                                  (string-upcase
                                   (var :aspect.subversion.checkout-strategy
                                        :fresh-copy)))))))

(define-aspect (trigger/scm) () ()
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

(defmethod builder-constraints ((aspect  aspect-builder-defining-mixin)
                                (builder t))
  (let* ((builder-type (type-of builder))
         (variable     (format-symbol :keyword "ASPECT.BUILDER-CONSTRAINTS.~@:(~A~)"
                                      (let ((type-string (string builder-type)))
                                        (subseq type-string (length "builder/")))))
         (value  (ignore-errors (value aspect variable))))
    (log:trace "Constraints for ~A in ~A: ~A" builder variable value)
    (mapcar (lambda+ ((kind &rest args))
              (list* (make-keyword (string-upcase kind))
                     (if (string= (first args) "<all>")
                         t
                         (intern (first args) #.*package*))
                     (rest args)))
            value)))

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

(define-aspect (slaves) () ()
  (setf (slaves job) (var :slaves)))

;;; Dependency download aspect

(define-aspect (dependency-download :job-var  job
                                    :spec-var spec)
    (builder-defining-mixin) ()
  (when-let ((dependencies (append
                            (mapcar (lambda (x) (value x :bla-name))
                                    (dependencies spec))
                            (var :aspect.dependency-download.dependencies))))
    ;; shell builder which unpacks dependencies.
    (push (constraint! (((:before cmake/unix)))
           (shell (:command #?"cd ${(var :upstream-dir)}
for archive in *.tar.gz ; do tar -xzf \"\${archive}\" ; done")))
          (builders job))

    ;; Multiple copy-artifact builders which copy artifacts from other
    ;; jobs.
    (iter (for dependency in dependencies)
          (push (constraint! (((:after sloccount)) copy-artifact)
                 (copy-artifact (:project-name #?"${dependency}/label=$label"
                                 :filter       #?"${(var :build-dir)}/*.tar.gz"
                                 :target       (var :upstream-dir)
                                 :flatten?     t
                                 :clazz        "hudson.plugins.copyartifact.StatusBuildSelector")))
                (builders job)))))

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
  (when-let ((command (var :aspect.shell.command nil)))
    (push (constraint! () (shell (:command command)))
          (builders job))))

;;; CMake aspects

(define-aspect (cmake/unix :job-var  job
                           :spec-var spec)
    (builder-defining-mixin) ()
  (let+ (((&flet shellify (name)
            (string-upcase (substitute #\_ #\- name))))
         (variables (iter (for variable in (var :aspect.cmake.environment '()))
                          (collect (format nil "export ~A~%" variable))))
         ((&flet+ format-option ((name value))
            (format nil "-D~A=~A \\~%" name value)))
         (dependencies (mapcar #'second
                               (requires-of-kind :cmake (specification (parent spec)))))
         (finds   (iter (for dependency in dependencies)
                        (collect #?"${(shellify dependency)}_DIR=\"\$(find \"${(var :dependency-dir)}\" -type f \\( -name \"${dependency}Config.cmake\" -o -name \"${(string-downcase dependency)}-config.cmake\" \\) -exec dirname {} \\;)\"\n")) )
         (options (mapcar
                   #'format-option
                   (append
                    (iter (for dependency in dependencies)
                          (collect (list (format nil "~A_DIR" dependency)
                                         #?"\${${(shellify dependency)}_DIR}")))

                    (mapcar (lambda (spec) (split-sequence #\= spec))
                            (var :aspect.cmake.options '())))))
         (targets (var :aspect.cmake.targets '()))
         (step    (shell (:command #?"mkdir -p ${(var :build-dir)} && cd ${(var :build-dir)}

@{variables}

@{finds}
cmake @{options} ..
make # not always necessary, but sometimes, sadly
make @{targets}" ))))

    (push (constraint! (((:after dependency-download)))
            step)
          (builders job))

    ;; Archive the generate tar.gz package
    (when (member "package" targets :test #'string=)
      (with-interface (publishers job) (archiver (publisher/archive-artifacts
                                                  :files        nil
                                                  :only-latest? nil))
        (pushnew #?"${(var :build-dir)}/*.tar.gz" (files archiver)
                 :test #'string=)))))

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

(define-aspect (cmake/cpp :job-var job) (cmake/unix) ()
  )

;;; Maven aspect

(define-aspect (maven :job-var job) (builder-defining-mixin) ()
  (push (constraint! ()
         (maven (:properties          (mapcan (lambda (spec)
                                                (let+ (((name value) (split-sequence #\= spec)))
                                                  (list (make-keyword name) value)))
                                              (var :aspect.maven.properties))
                 :targets             (var :aspect.maven.targets)
                 :private-repository? (var :aspect.maven.private-repository?)
                 :settings            (var :aspect.maven.settings-file :default)
                 :global-settings     (var :aspect.maven.global-settings-file :default))))
        (builders job)))

;;; Setuptools aspect

(define-aspect (setuptools :job-var job) (builder-defining-mixin) ()
  (let+ ((options '())
         ((&flet+ add-option ((section name value))
            (appendf options (list #?"\${PYTHON} ${(var :aspect.setuptools.script)} setopt -c ${section} -o ${name} -s \"${value}\"\n"))))
         (targets '())
         ((&flet+ add-target ((name &optional no-fail?))
            (let ((no-fail (when no-fail? '("|| true"))))
              (appendf options (list #?"\${PYTHON} ${(var :aspect.setuptools.script)} ${name} @{no-fail}\n"))))))
    (mapc #'add-option (var :aspect.setuptools.options))
    (mapc (compose #'add-target #'ensure-list)
          (var :aspect.setuptools.targets))

    (push (constraint! (((:after dependency-download)))
           (shell (:command #?"PYTHON=${(var :python.binary)}
mkdir -p \"${(var :python.site-packages-dir)}\"
export PYTHONPATH=\${PYTHONPATH}:\"${(var :python.site-packages-dir)}\"

@{options}

@{targets}")))
          (builders job))))

;;; Warnings aspect

(define-aspect (warnings :job-var job) () ()
  (with-interface (publishers job) (warnings (publisher/warnings))
    (iter (for parser in (var :warning-parsers))
          (pushnew (make-instance 'warning-parser/console :name parser)
                   (console-parsers warnings)
                   :test #'string=
                   :key  #'name))))

#+cpp (list "GNU Compiler 4 (gcc)"
      "Apple LLVM Compiler (Clang)")

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

(define-aspect (debian-package/cmake)
    (debian-package
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
