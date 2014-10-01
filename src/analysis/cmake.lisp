;;;; cmake.lisp ---
;;;;
;;;; Copyright (C) 2012, 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.analysis)

(defparameter *main-cmake-file-name* "CMakeLists.txt"
  "TODO(jmoringe): document")

(defparameter *set-version-scanner*
  (ppcre:create-scanner
   "^[ \\t]*set\\([ \\t\\n]*([_A-Z]*VERSION[_A-Z]*)[ \\t\\n]+\"?([$0-9][^ )\"]*)\"?[^)]*\\)"
   :multi-line-mode       t
   :case-insensitive-mode t)
  "TODO(jmoringe): document")

(defparameter *set-variable-scanner*
  (ppcre:create-scanner
   "^[ \\t]*set\\([ \\t\\n]*([^ \\t\\n]*)[ \\t\\n]+\"?([^ \\t\\n)\"]*)\"?[^)]*\\)"
   :multi-line-mode       t
   :case-insensitive-mode t)
  "TODO(jmoringe): document")

(defparameter *project-scanner*
  (ppcre:create-scanner
   "^[ \\t]*project\\([ \\t]*([^ )]*)[^)]*\\)"
   :multi-line-mode       t
   :case-insensitive-mode t)
  "TODO(jmoringe): document")

(defparameter *find-package-scanner*
  (ppcre:create-scanner
   "^[ \\t]*find_package\\([ ]*([^ )]*)(?:[ ]+\"?([$0-9][^ )\"]*)\"?)?[^)]*\\)"
   :multi-line-mode       t
   :case-insensitive-mode t)
  "TODO(jmoringe): document")

(defparameter *pkg-check-modules-scanner* ; TODO semantics: check requires all modules; search requires at least one
  (ppcre:create-scanner
   "^[ \\t]*pkg_(?:check|search)_modules?\\([ \\t\\n]*([^ \\t\\n)]*)(?:[ \\t\\n]+(?:REQUIRED|QUIET))*[ \\t\\n]*([^)]*)\\)"
   :multi-line-mode       t
   :case-insensitive-mode t)
  "TODO(jmoringe): document")

(mapc (lambda+ ((input expected))
        (assert (equalp expected
                        (nth-value 1 (ppcre:scan-to-strings
                                      *pkg-check-modules-scanner* input)))))
      '(("pkg_check_modules(FOO REQUIRED QUIET bar)"
         #("FOO" "bar"))
        ("pkg_check_modules(BIOROB_CPP REQUIRED biorob-cpp-0.3>=0.3.1)"
         #("BIOROB_CPP" "biorob-cpp-0.3>=0.3.1"))))

(defparameter *project-version-scanner*
  (ppcre:create-scanner
   "^[ \\t]*define_project_version\\(([_A-Z]+)[ \\t]+\"?([$0-9][^ )\"]*)\"?[ \\t]+\"?([$0-9][^ )\"]*)\"?[ \\t]+\"?([$0-9][^ )\"]*)\"?"
   :multi-line-mode       t
   :case-insensitive-mode t)
  "TODO(jmoringe): document")

(defun format-version (major &optional minor patch)
  (format nil "~D~{~@[.~D~]~}" major (list minor patch)))

(defun find-cmake-files (directory)
  (values
   (or (probe-file (merge-pathnames *main-cmake-file-name* directory))
       (error "~@<Could not find main ~S file in ~S.~@:>"
              *main-cmake-file-name* directory))
   (find-files (merge-pathnames "*/**/CMakeLists.txt.*" directory))))

(defun find-cmake-config-files (directory)
  "TODO(jmoringe): document"
  (append (find-files (merge-pathnames "**/*-config.cmake.*" directory))
          (find-files (merge-pathnames "**/*Config.cmake.*" directory))))

(defun find-pkg-config-template-files (directory)
  (find-files (merge-pathnames #P"**/*.pc.*" directory)))

(defun cmake-config-file->project-name/dont-normalize (pathname)
  (ppcre:regex-replace "(.*)(?:Config|-config)(?:\\..+)"
                       (pathname-name pathname)
                       "\\1"))

(defun cmake-config-file->project-name (pathname &rest args)
  (apply #'rs.f::normalize-name
         (ppcre:regex-replace "(.*)(?:Config|-config)(?:\\..+)"
                              (pathname-name pathname)
                              "\\1")
         args))

(defun extract-project-version (source)
  (ppcre:register-groups-bind (prefix major minor patch)
      (*project-version-scanner* source)
    (let+ (((&flet make-component (name value)
              (cons (format nil "~AVERSION_~A" prefix name) value))))
      (values (format-version major minor patch)
              (mapcar #'make-component
                      (list "MAJOR" "MINOR" "PATCH")
                      (list major minor patch))))))

(defun %resolve-cmake-version (spec versions)
  (iter (repeat 32)
        (while (or (find #\$ spec) (find #\@ spec)))
        (iter (for (name . value) in versions)
              (when value
                (setf spec (ppcre:regex-replace-all
                            (list :sequence "${" name "}") spec value))
                (setf spec (ppcre:regex-replace-all
                            (list :sequence "@" name "@") spec value))))
        (finally (return spec))))

(defmethod analyze ((directory pathname)
                    (kind      (eql :cmake))
                    &key)
  (let+ ((versions '())
         (variables '())
         ((&values main-file extra-files) (find-cmake-files directory))
         (pkg-config-template-files (find-pkg-config-template-files directory))
         (source (read-file-into-string* main-file))

         (config-files (find-cmake-config-files directory))

         ((&values project-version components)
          (extract-project-version source)))

    (values
     (list
      :versions
      (progn
        (iter (for component in components)
              (pushnew component versions :test #'string= :key #'car))

        (ppcre:do-register-groups (project) (*project-scanner* source)
          (pushnew (cons "CMAKE_PROJECT_NAME" project) variables :test #'equal)
          (pushnew (cons "PROJECT_NAME" project) variables :test #'equal))

        (let ((found-project-version? project-version)
              (length))
          (ppcre:do-register-groups (version value)
              (*set-version-scanner* source)
            (when (and (not found-project-version?)
                       (or  (not length)
                            (< (length version) length))
                       #+no (not (ppcre:scan "(_MAJOR|_MINOR|_PATCH)$" version))
                       (ends-with-subseq "VERSION" version :test #'string-equal)
                       (some (lambda (file)
                               (let* ((name1 (cmake-config-file->project-name file))
                                      (name2 (cmake-config-file->project-name file :separator nil)))
                                 (or (starts-with-subseq name1 (rs.f::normalize-name version))
                                     (starts-with-subseq name2 (rs.f::normalize-name version)))))
                             config-files))
              (setf length          (length version)
                    project-version value))
            (pushnew (cons version value) versions :test #'equal)))

        (ppcre:do-register-groups (key value)
            (*set-variable-scanner* source)
          (pushnew (cons key value) variables :test #'equal))

        (unless project-version
          (let ((major (cdr (find "VERSION_MAJOR$" versions
                                  :test #'ppcre:scan :key  #'car)))
                (minor (cdr (find "VERSION_MINOR$" versions
                                  :test #'ppcre:scan :key  #'car)))
                (patch (cdr (find "VERSION_PATCH$" versions
                                  :test #'ppcre:scan :key  #'car))))

            (when major
              (setf project-version (format-version major minor patch)))))

        `((:main ,(%resolve-cmake-version project-version versions))))

      :provides
      (append
       (iter (for file in config-files)
             (let* ((name            (cmake-config-file->project-name/dont-normalize file))
                    (name/lower-case (string-downcase name))
                    (version         (parse-version (%resolve-cmake-version project-version versions))))
               (collect (list :cmake name version))
               (unless (string= name/lower-case name)
                 (collect (list :cmake name/lower-case version)))))
       (iter (for file in pkg-config-template-files)
             (let* ((content (read-file-into-string file))
                    (content (%resolve-cmake-version content variables)))
               (when-let* ((result (with-input-from-string (stream content)
                                     (analyze stream :pkg-config :name (first (split-sequence #\. (pathname-name file))))))
                           (provides (getf result :provides)))
                 (appending provides)))))

      :requires
      (iter (for file in (cons main-file extra-files))
            (let ((content (read-file-into-string file)))
              (ppcre:do-register-groups (name version)
                  (*find-package-scanner* content)
                (collect (list* :cmake
                                (%resolve-cmake-version name versions)
                                (when version (list (parse-version (%resolve-cmake-version version versions)))))))
              (ppcre:do-register-groups (variable modules)
                  (*pkg-check-modules-scanner* content)
                (declare (ignore variable))
                (ppcre:do-matches-as-strings (module
                                              "(?:\"[^\"]*\"|[^ \\t\\n\"]+)"
                                              modules)
                  (let+ ((module (string-trim '(#\") module))
                         (resolved (%resolve-cmake-version module variables))
                         ((name &optional version)
                          (or (ppcre:register-groups-bind (name version)
                                  ("([^>=]+)>=(.*)" resolved)
                                (list name version))
                              (list resolved))))
                    (collect (list* :pkg-config name
                                    (when version (list (parse-version version))))))))))

      :properties
      (append
       (when-let ((license (analyze directory :license)))
         `((:license . ,license)))))

     versions)))



(defmethod analyze ((directory pathname)
                    (kind      (eql :cmake/debian))
                    &key)
  (let+ (((&values info versions) (analyze directory :cmake))

         ((&flet extract-field (name content)
            (when-let* ((regex (ppcre:create-scanner
                                (format nil "set\\(~A[ ]+\"?((?:[^,\"]+[, ]*)*)\"?\\)" name)
                                :multi-line-mode       t
                                :case-insensitive-mode t))
                        (temp  (nth-value 1 (ppcre:scan-to-strings regex content)))
                        (match (aref temp 0)))
              (mapcar (curry #'string-trim '(#\Space #\Tab #\Newline))
                      (split-sequence #\, match)))))
         ((&flet extract-dependencies (name kind content)
            (iter (for value in (extract-field name content))
                  (ppcre:register-groups-bind (name relation version)
                      ("([^ ]+)(?:[ ]+\\(([<>=]+)[ ]+([0-9.]+)\\))?" value)
                    (collect (list :debian
                                   (%resolve-cmake-version name versions)
                                   (when version
                                     (%resolve-cmake-version version versions))
                                   (when relation (find-symbol relation :cl))
                                   kind)))))))

    (append
     info
     (when-let* ((file (first
                        (find-files (merge-pathnames "c*/*ebian*.cmake" directory))))
                 (content (read-file-into-string file)))
       (list
        :dependencies/debian/depends
        (extract-dependencies "CPACK_DEBIAN_PACKAGE_DEPENDS" :depends content)
        :dependencies/debian/recommends
        (extract-dependencies "CPACK_DEBIAN_PACKAGE_RECOMMENDS" :recommends content)
        :dependencies/debian/suggests
        (extract-dependencies "CPACK_DEBIAN_PACKAGE_SUGGESTS" :suggests content))))))
