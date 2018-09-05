;;;; cmake.lisp ---
;;;;
;;;; Copyright (C) 2012-2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:jenkins.analysis.cmake
  (:use
   #:cl
   #:alexandria
   #:split-sequence
   #:let-plus
   #:iterate

   #:jenkins.version)

  (:import-from #:jenkins.analysis
   #:analyze

   #:read-file-into-string*
   #:find-files
   #:merge-dependencies
   #:effective-requires))

(cl:in-package #:jenkins.analysis.cmake)

(defparameter *main-cmake-file-name* "CMakeLists.txt"
  "The filename of the CMake file for a project.")

(defparameter *variable-reference-scanner*
  (ppcre:create-scanner
   (format nil "(?:~
                  \\$\\{([^$}]+)\\}~
                |~
                  @([^@]+)@~
                )"))
  "Finds reference of the forms ${…} and @…@.")

(defparameter *set-variable-scanner*
  (ppcre:create-scanner
   (format nil "^[ \\t]*set[ \\t\\n]*\\(~
                  [ \\t\\n]*((?!ENV{)[^ \\t\\n]*)~
                  [ \\t\\n]+\"?~
                    ([^ \\t\\n)\"]*)~
                  \"?~
                  [^)]*~
                \\)")
   :multi-line-mode       t
   :case-insensitive-mode t)
  "Finds set(<name> <value>) calls.")

(defparameter *project-scanner*
  (ppcre:create-scanner
   (format nil "^[ \\t]*project[ \\t\\n]*\\(~
                  [ \\t\\n]*\"?~
                    ([^ \\t\\n)\"]*)~
                  \"?~
                  (?:~
                    [^)]*~
                    version~
                    [ \\t\\n]+\"?~
                      ([^ \\t\\n)\"]*)~
                    \"?~
                  )?~
                  [^)]*~
                \\)")
   :multi-line-mode       t
   :case-insensitive-mode t)
  "Finds project(<name> …) calls.")

(assert (equalp #("foo" "1")
                (nth-value
                 1 (ppcre:scan-to-strings
                    *project-scanner* "project(foo version \"1\")"))))
(assert (equalp #("foo" "${BAR}")
                (nth-value
                 1 (ppcre:scan-to-strings
                    *project-scanner* "project(foo version ${BAR} LANGUAGES C CXX)"))))

(defparameter *subdirs-scanner*
  (ppcre:create-scanner
   (format nil "^[ \\t]*subdirs[ \\t\\n]*\\(~
                  ([^)]*)~
                \\)")
   :multi-line-mode       t
   :case-insensitive-mode t)
  "Finds subdirs(<name> …) calls.")

(defparameter *include-scanner*
  (ppcre:create-scanner
   (format nil "^[ \\t]*include[ \\t\\n]*\\(~
                  [ \\t\\n]*\"?~
                    ([^)\"]*)~
                  \"?~
                \\)")
   :multi-line-mode       t
   :case-insensitive-mode t)
  "Finds include(<name> …) calls.")

(defparameter *add-subdirectory-scanner*
  (ppcre:create-scanner
   (format nil "^[ \\t]*add_subdirectory[ \\t\\n]*\\(~
                  ([^)]*)~
                \\)")
   :multi-line-mode       t
   :case-insensitive-mode t)
  "Finds add_subdirectory(<name> …) calls.")

(defparameter *find-package-scanner*
  (ppcre:create-scanner
   (format nil "^[ \\t]*find_package[ \\t\\n]*\\(~
                  [ \\t\\n]*([-_.A-Za-z0-9${}]+)~
                  (?:~
                    [ \\t\\n]+~
                    \"?([$0-9][^ )\"]*)\"?~
                  )?~
                  (?:~
                    [^)]*?~
                    (?:[ \\t\\n]+COMPONENTS)?~
                    ((?:[ \\t\\n]+\"?~
                      (?!(?:EXACT|QUIET|REQUIRED|CONFIG|NO_MODULE|NO_POLICY_SCOPE~
                            |NAMES|CONFIGS|HINTS|PATHS|PATH_SUFFIXES~
                            |NO_DEFAULT_PATH|NO_CMAKE_ENVIRONMENT_PATH~
                            |NO_CMAKE_PATH|NO_SYSTEM_ENVIRONMENT_PATH~
                            |NO_CMAKE_PACKAGE_REGISTRY|NO_CMAKE_BUILDS_PATH~
                            |NO_CMAKE_SYSTEM_PATH~
                            |NO_CMAKE_SYSTEM_PACKAGE_REGISTRY~
                            |CMAKE_FIND_ROOT_PATH_BOTH~
                            |ONLY_CMAKE_FIND_ROOT_PATH~
                            |NO_CMAKE_FIND_ROOT_PATH)~
                         [ \\t\\n)])~
                      [^ \\t\\n)\"]+~
                    \"?)+)~
                  )?~
                  [^)]*~
                \\)")
   :multi-line-mode       t
   :case-insensitive-mode t)
  "Finds find_package(…) calls.")

(defparameter *pkg-check-modules-scanner* ; TODO semantics: check requires all modules; search requires at least one
  (ppcre:create-scanner
   (format nil "^[ \\t]*pkg_(?:check|search)_modules?[ \\t\\n]*\\(~
                  [ \\t\\n]*~
                    ([-_.A-Za-z0-9]+)~
                  (?:~
                    [ \\t\\n]+~
                      (?:REQUIRED|QUIET)~
                  )*~
                  ((?:~
                    [ \\t\\n]+\"?~
                      (?!(?:REQUIRED|QUIET)[ \\t\\n)])~
                      [^ \\t\\n)\"]+~
                    \"?~
                  )*)~
                  [^)]*~
                \\)")
   :multi-line-mode       t
   :case-insensitive-mode t)
  "Finds pkg_{check,search}_module(…) calls.")

(mapc (lambda+ ((input expected))
        (assert (equalp expected
                        (nth-value 1 (ppcre:scan-to-strings
                                      *pkg-check-modules-scanner* input)))))
      '(("pkg_check_modules(FOO REQUIRED QUIET bar)"
         #("FOO" " bar"))
        ("pkg_check_modules(BIOROB_CPP REQUIRED biorob-cpp-0.3>=0.3.1)"
         #("BIOROB_CPP" " biorob-cpp-0.3>=0.3.1"))
        ("PKG_CHECK_MODULES(GSTREAMER REQUIRED
           \"gstreamer-${GSTREAMER_VERSION_SUFFIX}\"
           \"gstreamer-base-${GSTREAMER_VERSION_SUFFIX}\"
           \"gstreamer-audio-${GSTREAMER_VERSION_SUFFIX}\")"
         #("GSTREAMER"
           "
           \"gstreamer-${GSTREAMER_VERSION_SUFFIX}\"
           \"gstreamer-base-${GSTREAMER_VERSION_SUFFIX}\"
           \"gstreamer-audio-${GSTREAMER_VERSION_SUFFIX}\""))
        ("pkg_check_modules(foo REQUIRED foo bar QUIET)"
         #("foo" " foo bar"))))

(defparameter *project-version-scanner*
  (ppcre:create-scanner
   (format nil "^[ \\t]*define_project_version\\(~
                  [ \\t\\n]*~
                    ([_A-Z]+)~
                  [ \\t\\n]+\"?~
                    ([$0-9][^ )\"]*)~
                  \"?~
                  [ \\t\\n]+\"?~
                    ([$0-9][^ )\"]*)~
                  \"?~
                  [ \\t\\n]+\"?~
                    ([$0-9][^ )\"]*)~
                  \"?")
   :multi-line-mode       t
   :case-insensitive-mode t)
  "Finds define_project_version(…) calls.")

(defun config-file->project-name (pathname)
  (let+ (((&values match? groups)
          (ppcre:scan-to-strings "(.+)(?:Config|-config)"
                                 (pathname-name pathname))))
    (when match?
      (aref groups 0))))

(defun format-version (major &optional minor patch)
  (format nil "~D~{~@[.~D~]~}" major (list minor patch)))

(defun extract-project-version (source)
  (ppcre:register-groups-bind (prefix major minor patch)
      (*project-version-scanner* source)
    (log:debug "~@<Found define_project_version(…) call with prefix ~S ~
                version components ~S ~S ~S~@:>"
              prefix major minor patch)
    (let+ (((&flet make-component (name value)
              (list (format nil "~AVERSION~@[_~A~]" prefix name) value)))
           (version (format-version major minor patch)))
      (values version
              (mapcar #'make-component
                      (list "MAJOR" "MINOR" "PATCH" nil)
                      (list major   minor   patch   version))))))

(defclass environment (print-items:print-items-mixin)
  ((%parent    :initarg  :parent
               :reader   parent
               :initform nil)
   (%variables :reader   %variables
               :initform (make-hash-table :test #'equal))))

(defmethod print-items:print-items append ((object environment))
  (let+ (((&labels depth (environment)
            (if-let ((parent (parent environment)))
              (1+ (depth parent))
              0)))
         (variable-count (hash-table-count (%variables object))))
    `((:variable-count ,variable-count "~:D variable~:P")
      (:depth          ,(depth object) " @~D"            ((:after :variable-count))))))

(defmethod lookup ((name string) (environment environment))
  (or (gethash name (%variables environment))
      (when-let ((parent (parent environment)))
        (lookup name parent))))

(defmethod (setf lookup) ((new-value   t)
                          (name        string)
                          (environment environment))
  (setf (gethash name (%variables environment)) new-value))

(defmethod augment! ((environment environment)
                     (entries     list))
  (loop :for (name . value) :in entries
        :do (setf (lookup name environment) value))
  environment)

(defun %resolve-variables (spec environment &key (if-unresolved :partial))
  (flet ((replace-all (string)
           (let ((complete? t))
             (multiple-value-call #'values
               (ppcre:regex-replace-all
                *variable-reference-scanner* string
                (lambda (string start end match-start match-end group-starts group-ends)
                  (declare (ignore start end))
                  (let ((name  (cond ((when-let ((start (aref group-starts 0))) ; ${…}
                                        (subseq string start (aref group-ends 0))))
                                     ((when-let ((start (aref group-starts 1))) ; @…@
                                        (subseq string start (aref group-ends 1)))))))
                    (or (lookup name environment)
                        (progn
                          (setf complete? nil)
                          (subseq string match-start match-end))))))
               complete?))))
    (loop :repeat 32
          :for (result match? complete?) = (list spec t t) :then (multiple-value-list
                                                                  (replace-all result))
          :while match?
          :finally (return (cond (complete?
                                  result)
                                 ((functionp if-unresolved)
                                  (funcall if-unresolved spec))
                                 ((eq if-unresolved :partial)
                                  result)
                                 (t
                                  if-unresolved))))))

(let* ((string1   "${fo${fez}}${bar}${baz}")
       (string2   "${fo${fez}}${bar}${ba}")
       (variables '(("foo" . "1") ("bar" . "2") ("baz" . "3") ("fez" . "o")))
       (environment (augment! (make-instance 'environment) variables)))
  (assert (string= (%resolve-variables string1 environment)                         "123"))
  (assert (string= (%resolve-variables string2 environment)                         "12${ba}"))
  (assert (string= (%resolve-variables string2 environment :if-unresolved :partial) "12${ba}"))
  (assert (eq      (%resolve-variables string2 environment :if-unresolved :foo)     :foo))
  (assert (null (ignore-errors
                 (%resolve-variables
                  string2 environment :if-unresolved (rcurry #'%resolution-error ""))))))

(defun %resolution-error (thing context)
  (error "~@<Could not resolve ~S in ~A.~@:>"
         thing context))

(defun %continuable-resolution-error (context report)
  (lambda (expression)
    (if report
        (with-simple-restart (continue report)
          (%resolution-error expression context))
        (%resolution-error expression context))))

(defmethod analyze ((source pathname) (kind (eql :cmake))
                    &key)
  (let ((result (analyze source :cmake/one-directory :implicit-provides? t)))
    ;; Remove the internal-use :secondary-files property.
    (remove-from-plist result :secondary-files)))

(defmethod analyze ((source pathname) (kind (eql :cmake/one-directory))
                    &key
                    parent-environment
                    implicit-provides?)
  (log:info "~@<Analyzing directory ~S~@:>" source)
  (let+ ((lists-file (merge-pathnames *main-cmake-file-name* source))
         ((&values (&plist-r/o (provides :provides) (requires :requires))
                   environment
                   sub-directories)
          (analyze lists-file :cmake/one-file
                   :parent-environment parent-environment
                   :implicit-provides? implicit-provides?))
         (project-version (third (first provides)))
         ;; Analyze sub-projects.
         (sub-projects
          (mapcan (lambda (directory)
                    (when (uiop:directory-exists-p directory)
                      (with-simple-restart
                          (continue "~@<Skip sub-directory ~S~@:>" directory)
                        (list (analyze directory kind
                                       :parent-environment environment)))))
                  sub-directories))
         ((&flet property-values (name)
            (loop :for project       in sub-projects
                  :for project-name  = (second (first (getf project :provides)))
                  :for project-value = (getf project name)
                  :when project-value
                  :collect (list project-name project-value))))
         ;; Append the values in the analyzed projects.
         ((&flet property-value/append (name)
            (remove-duplicates (reduce #'append (property-values name)
                                       :key #'second)
                               :test #'equal)))
         ((&flet property-value/dependencies (name &key (test #'version>=))
            (let ((all (reduce #'append (property-values name) :key #'second)))
              (merge-dependencies all :test test))))
         (sub-provides        (property-value/dependencies :provides))
         (sub-requires        (property-value/dependencies :requires))
         (sub-secondary-files (property-value/append :secondary-files))
         ;; Analyze CMake config-mode template files and pkg-config
         ;; template files not already covered by sub-projects.
         ((&flet analyze-secondary (files kind)
            (iter (for file in files)
                  (log:info "~@<Analyzing secondary file ~S~@:>" file)
                  (with-simple-restart (continue "Skip file ~S" file)
                    (appending (analyze file kind
                                        :environment     environment
                                        :project-version project-version))))))
         (config-mode-files    (%find-config-mode-templates source))
         (config-mode-provides (analyze-secondary
                                (set-difference
                                 config-mode-files sub-secondary-files
                                 :test #'equalp)
                                :cmake/config-mode-template))
         (pkg-config-files     (%find-pkg-config-template-files source))
         (pkg-config-provides  (analyze-secondary
                                (set-difference
                                 pkg-config-files sub-secondary-files
                                 :test #'equalp)
                                :cmake/pkg-config-template))
         ;; Compute effective requires and provides.
         (provides             (merge-dependencies
                                (append provides sub-provides
                                        config-mode-provides
                                        pkg-config-provides)))
         (requires             (merge-dependencies
                                (append requires sub-requires))))
    `(:natures               (:cmake)
      :provides              ,provides
      :requires              ,(effective-requires requires provides)
      :programming-languages ("C++") ; TODO actual analysis
      :secondary-files       ,(append config-mode-files pkg-config-files))))

(defmethod analyze ((source pathname)
                    (kind   (eql :cmake/one-file))
                    &key
                    (parent-environment '())
                    (environment        (make-instance 'environment :parent parent-environment))
                    implicit-provides?)
  (let+ ((content (read-file-into-string* source))
         ((&values project-version components)
          (extract-project-version content))
         ((&flet add-variable! (name value)
            (log:trace "~@<Adding variable ~A = ~A~@:>" name value)
            (setf (lookup name environment) value)))
         ((&flet add-project-variable! (name value)
            (add-variable! name                         value)
            (add-variable! (format nil "CMAKE_~A" name) value)))
         ((&flet resolve (expression)
            (%resolve-variables
             expression environment :if-unresolved nil)))
         (included-requires '()))
    ;; Set CMAKE_SOURCE_DIR to the current directory in case an
    ;; include(…) call or similar depends on it.
    (add-variable! "CMAKE_SOURCE_DIR"
                   (namestring (uiop:pathname-directory-pathname source)))

    ;; Collect all variables directly defined in SOURCE.
    (ppcre:do-register-groups (key value) (*set-variable-scanner* content)
      (add-variable! key value))

    ;; Collect variables for project(…) calls.
    (ppcre:do-register-groups (project version) (*project-scanner* content)
      (log:info "~@<Found project(…) call with name ~S~@[ and version ~S~]~@:>"
                project version)
      (add-project-variable! "PROJECT_SOURCE_DIR" (namestring
                                                   (uiop:pathname-directory-pathname
                                                    source)))
      (add-project-variable! "PROJECT_NAME"       project)
      (when version
        (add-project-variable! "PROJECT_VERSION" version)
        (loop :for name      :in '("MAJOR" "MINOR" "PATH")
              :for value     :in (parse-version version)
              :for variable  =   (format nil "PROJECT_VERSION_~A" name)
              :do  (add-project-variable! variable (princ-to-string value)))))
    ;; If we found a define_project_version(…) call, use the versions
    ;; defined there.
    (mapc (curry #'apply #'add-variable!) components)

    ;; Collect requirements in files included via include(…) calls.
    (ppcre:do-register-groups (include) (*include-scanner* content)
      (let ((label include))
        (with-simple-restart
            (continue "~@<Ignore included file ~S.~@:>" label)
          (when-let* ((resolved (%resolve-variables
                                 include environment
                                 :if-unresolved (rcurry #'%resolution-error
                                                        "include(…) expression")))
                      (filename (unless (emptyp resolved)
                                  (merge-pathnames resolved source)))
                      (results  (when (probe-file filename)
                                  (setf label resolved)
                                  (analyze filename kind
                                           :environment environment))))
            (appendf included-requires (getf results :requires))))))

    ;; Compute and resolve name and version, analyze find_package(…)
    ;; and pkg_(search|check)_module(…) calls.
    (let+ ((name/resolved    (when implicit-provides?
                               (resolve "${CMAKE_PROJECT_NAME}")))
           ((&flet resolve-project-variable (name)
              (or (resolve (format nil "${~A_~A}" "CMAKE_PROJECT" name))
                  (resolve (format nil "${~:@(~A~)_~A}" name/resolved name)))))
           (version          (when implicit-provides?
                               (or project-version
                                   (when name/resolved
                                     (or (resolve-project-variable "VERSION")
                                         (let+ (((major minor patch)
                                                 (map 'list #'resolve-project-variable
                                                      '("VERSION_MAJOR"
                                                        "VERSION_MINOR"
                                                        "VERSION_PATCH"))))
                                           (when major
                                             (format-version major minor patch))))))))
           (version/resolved (when implicit-provides?
                               (resolve version))))
      (values
       (list
        :provides (when name/resolved
                    (list (%make-dependency name/resolved version/resolved)))
        :requires (merge-dependencies
                   (append (%analyze-find-packages content environment)
                           (%analyze-pkg-configs content environment)
                           included-requires)))
       environment
       (%analyze-sub-directories
        content environment
        (uiop:pathname-directory-pathname source))))))

(defun %analyze-sub-directory (kind arguments environment directory)
  (let+ ((description (ecase kind
                        (:subdirs          "subdirs(…) expression")
                        (:add-subdirectory "add_subdirectory(…) expression")))
         ((&flet resolve (expression &optional continue-report)
            (%resolve-variables
             expression environment
             :if-unresolved (%continuable-resolution-error
                             description continue-report))))
         (arguments          (%split-arguments arguments))
         (arguments/relevant (ecase kind
                               (:subdirs          arguments)
                               (:add-subdirectory (subseq arguments 0 1))))
         (arguments/resolved (mapcar (rcurry #'resolve "Ignore the module")
                                     arguments/relevant)))
    (log:debug "~@<Found ~A with director~@P ~{~{~S~^ → ~S~}~^, ~
                ~}~@:>"
               description (length arguments)
               (mapcar #'%list-unless-equal
                       arguments arguments/resolved))
    (mapcan (lambda (sub-directory)
              (when (and sub-directory (not (emptyp sub-directory)))
                (list (merge-pathnames
                       (make-pathname :directory `(:relative ,sub-directory))
                       directory))))
            arguments/resolved)))

(defun %analyze-sub-directories (content environment directory)
  (let ((result '()))
    (ppcre:do-register-groups (arguments)
        (*subdirs-scanner* content)
      (with-simple-restart (continue "Skip the dependency")
        (appendf result (%analyze-sub-directory
                         :subdirs arguments environment directory))))
    (ppcre:do-register-groups (arguments)
        (*add-subdirectory-scanner* content)
      (with-simple-restart (continue "Skip the dependency")
        (appendf result (%analyze-sub-directory
                         :add-subdirectory arguments environment directory))))
    result))

(defun %analyze-find-package (name version components environment)
  (let+ (((&flet resolve (expression &optional continue-report)
            (%resolve-variables
             expression environment
             :if-unresolved (%continuable-resolution-error
                             "find_package(…) expression"
                             continue-report))))
         (name/resolved       (resolve name))
         (version/resolved    (when version
                                (resolve version "Treat the dependency as not versioned")))
         (components          (%split-arguments components))
         (components/resolved (mapcar (rcurry #'resolve "Ignore the component")
                                      components)))
    (log:debug "~@<Found find_package(…) call with ~
                ~{~S~^ → ~S~}~
                ~@[ version ~{~S~^ → ~S~}~]~
                ~@[ components ~{~{~S~^ → ~S~}~^, ~}~]~@:>"
               (%list-unless-equal name name/resolved)
               (%list-unless-equal version version/resolved)
               (mapcar #'%list-unless-equal
                       components components/resolved))
    (if (string-equal name "catkin")
        (mapcar #'%make-dependency (remove nil components/resolved))
        (list (%make-dependency name/resolved version/resolved)))))

(defun %analyze-find-packages (content environment)
  (let ((result '()))
    (ppcre:do-register-groups (name version components)
        (*find-package-scanner* content)
      (with-simple-restart (continue "Skip the dependency")
        (appendf result (%analyze-find-package
                         name version components environment))))
    result))

(defun %parse-pkg-config-module (spec)
  (ppcre:register-groups-bind (name version) ("([^>=]+)>=(.*)" spec)
    (return-from %parse-pkg-config-module (values name version)))
  spec)

(defun %analyze-pkg-config (variable modules environment)
  (let+ (((&flet resolve (expression &optional continue-report)
            (%resolve-variables
             expression environment
             :if-unresolved (%continuable-resolution-error
                             "pkg_(search|check)_module(…) expression"
                             continue-report))))
         (modules          (%split-arguments modules))
         (modules/resolved (mapcar (rcurry #'resolve "Ignore the module")
                                   modules)))
    (log:debug "~@<Found pkg_(search|check)_module(…) call with ~
                output ~S modules ~{~{~S~^ → ~S~}~^, ~}~@:>"
               variable
               (mapcar #'%list-unless-equal modules modules/resolved))
    (mapcan (lambda (module)
              (when module
                (with-simple-restart (continue "Skip module ~S" module)
                  (let+ (((&values name version)
                          (%parse-pkg-config-module module)))
                    (list (%make-dependency name version :pkg-config))))))
            modules/resolved)))

(defun %analyze-pkg-configs (content environment)
  (let ((result '()))
    (ppcre:do-register-groups (variable modules)
        (*pkg-check-modules-scanner* content)
      (with-simple-restart (continue "Skip the dependency")
        (appendf result (%analyze-pkg-config variable modules environment))))
    result))

;;; CMake Config-mode Template Files

(defun %find-config-mode-templates (directory)
  (append (find-files (merge-pathnames "**/*-config.cmake.*" directory))
          (find-files (merge-pathnames "**/*Config.cmake.*" directory))))

(defmethod analyze ((source pathname)
                    (kind   (eql :cmake/config-mode-template))
                    &key
                    project-version)
  (when-let ((name (config-file->project-name source)))
    (%make-dependencies name :version project-version)))

;;; pkg-config Template Files

(defun %find-pkg-config-template-files (directory)
  (find-files (merge-pathnames #P"**/*.pc.*" directory)))

(defmethod analyze ((source pathname)
                    (kind   (eql :cmake/pkg-config-template))
                    &key
                    environment)
  (let* ((name    (first (split-sequence #\. (pathname-name source))))
         (content (read-file-into-string source))
         (content (%resolve-variables content environment)))
    (when-let* ((result (with-input-from-string (stream content)
                          (analyze stream :pkg-config :name name))))
      (getf result :provides))))

;;; Utility functions

(defun %make-dependency (name &optional version (nature :cmake))
  (list* nature name (typecase version
                       (string (list (parse-version version)))
                       (cons   (list version)))))

(defun %make-dependencies (name
                           &key
                           version
                           (add-lower-case? t)
                           (add-upper-case? t))
  (let ((name/lower-case (when add-lower-case?
                           (string-downcase name)))
        (name/upper-case (when add-upper-case?
                           (string-upcase name))))
    (append
     (list (%make-dependency name version))
     (when (and name/lower-case (string/= name/lower-case name))
       (list (%make-dependency name/lower-case version)))
     (when (and name/upper-case (string/= name/upper-case name))
       (list (%make-dependency name/upper-case version))))))

(defun %project-version-from-variables (versions)
  (let+ (((&flet find-component (name)
            (cdr (find (format nil "VERSION_~A" name) versions
                       :test #'ends-with-subseq :key #'car))))
         ((major minor patch)
          (mapcar #'find-component '("MAJOR" "MINOR" "PATCH"))))
    (when major
      (log:debug "~@<Found version in variables ~A~@[.~A~@[.~A~]~]~@:>"
                 major minor patch)
      (format-version major minor patch))))

(defun %split-arguments (arguments)
  (let ((result '()))
    (ppcre:do-matches-as-strings
        (argument "(?:\"[^\"]*\"|[^ \\t\\n\"]+)" arguments)
      (push (string-trim '(#\") argument) result))
    (nreverse result)))

(defun %list-unless-equal (first second)
  (when first
    (list* first (unless (equal first second) (list second)))))
