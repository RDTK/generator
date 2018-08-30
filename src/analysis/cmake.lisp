;;;; cmake.lisp ---
;;;;
;;;; Copyright (C) 2012-2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.analysis)

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
                    [ \\t\\n]*\"?~
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

(defun cmake-config-file->project-name (pathname)
  (ppcre:regex-replace "(.*)(?:Config|-config)(?:\\..+)"
                       (pathname-name pathname)
                       "\\1"))

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

(defun %cmake-resolve-variables (spec variables &key (if-unresolved :partial))
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
                    (or (assoc-value variables name :test #'string=)
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

(let ((string1   "${fo${fez}}${bar}${baz}")
      (string2   "${fo${fez}}${bar}${ba}")
      (variables '(("foo" . "1") ("bar" . "2") ("baz" . "3") ("fez" . "o"))))
  (assert (string= (%cmake-resolve-variables string1 variables)                         "123"))
  (assert (string= (%cmake-resolve-variables string2 variables)                         "12${ba}"))
  (assert (string= (%cmake-resolve-variables string2 variables :if-unresolved :partial) "12${ba}"))
  (assert (eq      (%cmake-resolve-variables string2 variables :if-unresolved :foo)     :foo))
  (assert (null (ignore-errors
                 (%cmake-resolve-variables
                  string2 variables :if-unresolved (rcurry #'%cmake-resolution-error ""))))))

(defun %cmake-resolution-error (thing context)
  (error "~@<Could not resolve ~S in ~A.~@:>"
         thing context))

(defun %cmake-continuable-resolution-error (context report)
  (lambda (expression)
    (if report
        (with-simple-restart (continue report)
          (%cmake-resolution-error expression context))
        (%cmake-resolution-error expression context))))

(defmethod analyze ((source pathname) (kind (eql :cmake))
                    &key)
  ;; Remove the internal-use :secondary-files property.
  (remove-from-plist (analyze source :cmake/one-directory) :secondary-files))

(defmethod analyze ((source pathname) (kind (eql :cmake/one-directory))
                    &key
                    parent-variables)
  (log:info "~@<Analyzing directory ~S~@:>" source)
  (let+ ((lists-file (merge-pathnames *main-cmake-file-name* source))
         ((&values (&plist-r/o (provides :provides) (requires :requires))
                   variables
                   sub-directories)
          (analyze lists-file :cmake/one-file
                   :parent-variables parent-variables))
         (project-version (third (first provides)))
         ;; Analyze sub-projects.
         (sub-projects
          (mapcan (lambda (directory)
                    (when (uiop:directory-exists-p directory)
                      (with-simple-restart
                          (continue "~@<Skip sub-directory ~S~@:>" directory)
                        (list (analyze directory kind
                                       :parent-variables   variables
                                       :implicit-provides? nil)))))
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
                                        :variables       variables
                                        :project-version project-version))))))
         (config-mode-files    (%cmake-find-config-mode-templates source))
         (config-mode-provides (analyze-secondary
                                (set-difference
                                 config-mode-files sub-secondary-files
                                 :test #'equalp)
                                :cmake/config-mode-template))
         (pkg-config-files     (%cmake-find-pkg-config-template-files source))
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
                    (parent-variables   '())
                    (implicit-provides? t))
  (let+ ((content (read-file-into-string* source))
         ((&values project-version components)
          (extract-project-version content))
         (variables parent-variables)
         ((&flet add-variable! (name value)
            (log:trace "~@<Adding variable ~A = ~A~@:>" name value)
            (push (cons name value) variables)))
         ((&flet add-project-variable! (name value)
            (add-variable! name                         value)
            (add-variable! (format nil "CMAKE_~A" name) value)))
         ((&flet find-variable (name &key (test #'string=))
            (cdr (find name variables :test test :key #'car))))
         (included-requires '()))
    ;; Set CMAKE_SOURCE_DIR to the current directory in case an
    ;; include(…) call or similar depends on it.
    (add-variable! "CMAKE_SOURCE_DIR"
                   (uiop:pathname-directory-pathname source))

    ;; Collect all variables directly defined in SOURCE.
    (ppcre:do-register-groups (key value) (*set-variable-scanner* content)
      (add-variable! key value))

    ;; Collect variables for project(…) calls.
    (ppcre:do-register-groups (project version) (*project-scanner* content)
      (log:debug "~@<Found project(…) call with name ~S~@[ and version ~S~]~@:>"
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
          (let+ ((resolved (%cmake-resolve-variables
                            include variables
                            :if-unresolved (rcurry #'%cmake-resolution-error
                                                   "include(…) expression")))
                 (filename (merge-pathnames resolved source))
                 ((&values results variables)
                  (when (probe-file filename)
                    (setf label resolved)
                    (analyze filename kind
                             :parent-variables   variables
                             :implicit-provides? nil))))
            (appendf included-requires (getf results :requires))
            (loop :for (name . value) :in variables
                  :do (add-variable! name value))))))

    ;; Compute and resolve name and version, analyze find_package(…)
    ;; and pkg_(search|check)_module(…) calls.
    (let+ (((&flet find/suffix (name)
              (find-variable name :test #'ends-with-subseq)))
           (name/resolved    (when implicit-provides?
                               (%cmake-resolve-variables
                                "${CMAKE_PROJECT_NAME}" variables
                                :if-unresolved nil)))
           (version          (when implicit-provides?
                               (or project-version
                                   (find/suffix "VERSION")
                                   (let+ (((major minor patch)
                                           (mapcar #'find/suffix
                                                   '("VERSION_MAJOR"
                                                     "VERSION_MINOR"
                                                     "VERSION_PATCH"))))
                                     (when major
                                       (format-version major minor patch))))))
           (version/resolved (when implicit-provides?
                               (%cmake-resolve-variables
                                version variables :if-unresolved nil))))
      (values
       (list
        :provides (when name/resolved
                    (list (%cmake-make-dependency name/resolved version/resolved)))
        :requires (merge-dependencies
                   (append (%cmake-analyze-find-packages content variables)
                           (%cmake-analyze-pkg-configs content variables)
                           included-requires)))
       variables
       (%cmake-analyze-sub-directories
        content variables
        (uiop:pathname-directory-pathname source))))))

(defun %cmake-analyze-sub-directory (kind arguments variables directory)
  (let+ ((description (ecase kind
                        (:subdirs          "subdirs(…) expression")
                        (:add-subdirectory "add_subdirectory(…) expression")))
         ((&flet resolve (expression &optional continue-report)
            (%cmake-resolve-variables
             expression variables
             :if-unresolved (%cmake-continuable-resolution-error
                             description continue-report))))
         (arguments          (%cmake-split-arguments arguments))
         (arguments/relevant (ecase kind
                               (:subdirs          arguments)
                               (:add-subdirectory (subseq arguments 0 1))))
         (arguments/resolved (mapcar (rcurry #'resolve "Ignore the module")
                                     arguments/relevant)))
    (log:debug "~@<Found ~A with director~@P ~{~{~S~^ → ~S~}~^, ~
                ~}~@:>"
               description (length arguments)
               (mapcar #'%cmake-list-unless-equal
                       arguments arguments/resolved))
    (mapcan (lambda (sub-directory)
              (when (and sub-directory (not (emptyp sub-directory)))
                (list (merge-pathnames
                       (make-pathname :directory `(:relative ,sub-directory))
                       directory))))
            arguments/resolved)))

(defun %cmake-analyze-sub-directories (content variables directory)
  (let ((result '()))
    (ppcre:do-register-groups (arguments)
        (*subdirs-scanner* content)
      (with-simple-restart (continue "Skip the dependency")
        (appendf result (%cmake-analyze-sub-directory
                         :subdirs arguments variables directory))))
    (ppcre:do-register-groups (arguments)
        (*add-subdirectory-scanner* content)
      (with-simple-restart (continue "Skip the dependency")
        (appendf result (%cmake-analyze-sub-directory
                         :add-subdirectory arguments variables directory))))
    result))

(defun %cmake-analyze-find-package (name version components variables)
  (let+ (((&flet resolve (expression &optional continue-report)
            (%cmake-resolve-variables
             expression variables
             :if-unresolved (%cmake-continuable-resolution-error
                             "find_package(…) expression"
                             continue-report))))
         (name/resolved       (resolve name))
         (version/resolved    (when version
                                (resolve version "Treat the dependency as not versioned")))
         (components          (%cmake-split-arguments components))
         (components/resolved (mapcar (rcurry #'resolve "Ignore the component")
                                      components)))
    (log:debug "~@<Found find_package(…) call with ~
                ~{~S~^ → ~S~}~
                ~@[ version ~{~S~^ → ~S~}~]~
                ~@[ components ~{~{~S~^ → ~S~}~^, ~}~]~@:>"
               (%cmake-list-unless-equal name name/resolved)
               (%cmake-list-unless-equal version version/resolved)
               (mapcar #'%cmake-list-unless-equal
                       components components/resolved))
    (if (string-equal name "catkin")
        (mapcar #'%cmake-make-dependency (remove nil components/resolved))
        (list (%cmake-make-dependency name/resolved version/resolved)))))

(defun %cmake-analyze-find-packages (content variables)
  (let ((result '()))
    (ppcre:do-register-groups (name version components)
        (*find-package-scanner* content)
      (with-simple-restart (continue "Skip the dependency")
        (appendf result (%cmake-analyze-find-package
                         name version components variables))))
    result))

(defun %cmake-parse-pkg-config-module (spec)
  (ppcre:register-groups-bind (name version) ("([^>=]+)>=(.*)" spec)
    (return-from %cmake-parse-pkg-config-module (values name version)))
  spec)

(defun %cmake-analyze-pkg-config (variable modules variables)
  (let+ (((&flet resolve (expression &optional continue-report)
            (%cmake-resolve-variables
             expression variables
             :if-unresolved (%cmake-continuable-resolution-error
                             "pkg_(search|check)_module(…) expression"
                             continue-report))))
         (modules          (%cmake-split-arguments modules))
         (modules/resolved (mapcar (rcurry #'resolve "Ignore the module")
                                   modules)))
    (log:debug "~@<Found pkg_(search|check)_module(…) call with ~
                output ~S modules ~{~{~S~^ → ~S~}~^, ~}~@:>"
               variable
               (mapcar #'%cmake-list-unless-equal
                       modules modules/resolved))
    (mapcan (lambda (module)
              (when module
                (with-simple-restart (continue "Skip module ~S" module)
                  (let+ (((&values name version)
                          (%cmake-parse-pkg-config-module module)))
                    (list (%cmake-make-dependency name version :pkg-config))))))
            modules/resolved)))

(defun %cmake-analyze-pkg-configs (content variables)
  (let ((result '()))
    (ppcre:do-register-groups (variable modules)
        (*pkg-check-modules-scanner* content)
      (with-simple-restart (continue "Skip the dependency")
        (appendf result (%cmake-analyze-pkg-config
                         variable modules variables))))
    result))

;;; CMake Config-mode Template Files

(defun %cmake-find-config-mode-templates (directory)
  (append (find-files (merge-pathnames "**/*-config.cmake.*" directory))
          (find-files (merge-pathnames "**/*Config.cmake.*" directory))))

(defmethod analyze ((source pathname)
                    (kind   (eql :cmake/config-mode-template))
                    &key
                    project-version)
  (let ((name (cmake-config-file->project-name source)))
    (%cmake-make-dependencies name :version project-version)))

;;; pkg-config Template Files

(defun %cmake-find-pkg-config-template-files (directory)
  (find-files (merge-pathnames #P"**/*.pc.*" directory)))

(defmethod analyze ((source pathname)
                    (kind   (eql :cmake/pkg-config-template))
                    &key
                    variables)
  (let* ((name    (first (split-sequence #\. (pathname-name source))))
         (content (read-file-into-string source))
         (content (%cmake-resolve-variables content variables)))
    (when-let* ((result (with-input-from-string (stream content)
                          (analyze stream :pkg-config :name name))))
      (getf result :provides))))

;;; Utility functions

(defun %cmake-make-dependency (name &optional version (nature :cmake))
  (list* nature name (typecase version
                       (string (list (parse-version version)))
                       (cons   (list version)))))

(defun %cmake-make-dependencies (name
                                 &key
                                 version
                                 (add-lower-case? t)
                                 (add-upper-case? t))
  (let ((name/lower-case (when add-lower-case?
                           (string-downcase name)))
        (name/upper-case (when add-upper-case?
                           (string-upcase name))))
    (append
     (list (%cmake-make-dependency name version))
     (when (and name/lower-case (string/= name/lower-case name))
       (list (%cmake-make-dependency name/lower-case version)))
     (when (and name/upper-case (string/= name/upper-case name))
       (list (%cmake-make-dependency name/upper-case version))))))

(defun %cmake-project-version-from-variables (versions)
  (let+ (((&flet find-component (name)
            (cdr (find (format nil "VERSION_~A" name) versions
                       :test #'ends-with-subseq :key #'car))))
         ((major minor patch)
          (mapcar #'find-component '("MAJOR" "MINOR" "PATCH"))))
    (when major
      (log:debug "~@<Found version in variables ~A~@[.~A~@[.~A~]~]~@:>"
                 major minor patch)
      (format-version major minor patch))))

(defun %cmake-split-arguments (arguments)
  (let ((result '()))
    (ppcre:do-matches-as-strings
        (argument "(?:\"[^\"]*\"|[^ \\t\\n\"]+)" arguments)
      (push (string-trim '(#\") argument) result))
    (nreverse result)))

(defun %cmake-list-unless-equal (first second)
  (when first
    (list* first (unless (equal first second) (list second)))))
