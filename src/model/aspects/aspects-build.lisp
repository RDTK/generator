;;;; aspects-build.lisp --- Definitions of builder-creating aspects
;;;;
;;;; Copyright (C) 2012-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.model.aspects)

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

(define-variable :aspect.cmake/unix.find-commands list ; :write
  "Shell commands for finding upstream CMake packages.

   A list of strings of the form

     UPSTREAM_DIR=$(find ... -name UPSTREAMConfig.cmake ...)

   where UPSTREAM is the name of the respective upstream CMake
   module.")

(define-variable :aspect.cmake/unix.dir-options list ; :write
  "CMake commandline options for configuring upstream packages.

   A list of strings of the form

     UPSTREAM_DIR=${UPSTREAM_DIR}

   where UPSTREAM is the name of the respective upstream CMake
   module.")

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

  (defmethod lookup ((thing aspect-cmake/unix)
                     (name  (eql :aspect.cmake/unix.find-commands))
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

  (defmethod lookup ((thing aspect-cmake/unix)
                     (name  (eql :aspect.cmake/unix.dir-options))
                     &key if-undefined)
    (declare (ignore if-undefined))
    (let+ (((&flet make-option (required)
              (format nil "~A_DIR=\\${~A_DIR}"
                      required (shellify required)))))
      (result name (map-cmake-requirements #'make-option thing)))))

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
  (push (constraint! (build ((:after dependency-download)))
          (batch (:command command)))
        (builders job)))

;;; Maven aspect

(defun string/name=value? (thing)
  (and (stringp thing) (position #\= thing)))

(deftype string/name=value ()
  '(and string (satisfies string/name=value?)))

(defun every-string/name=value (thing)
  (and (listp thing) (every (of-type 'string/name=value) thing)))

(defun split-option (spec)
  (let ((position (position #\= spec)))
    (unless position
      (error "~@<Option ~S is not of the form NAME=VALUE.~@:>"
             spec))
    (list (subseq spec 0 position) (subseq spec (1+ position)))))

(define-aspect (maven :job-var job) (builder-defining-mixin)
    (((properties           '()) :type (list-of string/name=value)
      :documentation
      "A list of Maven properties that should be set for the build.
       Entries are of the form NAME=VALUE.")
     (targets                    :type (list-of string)
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

(deftype setuptools-option ()
  '(cons string (cons string (cons string null))))

(defun every-setuptools-option (thing)
  (and (listp thing) (every (of-type 'setuptools-option) thing)))

(define-aspect (setuptools :job-var job) (builder-defining-mixin)
    (((options '()) :type (list-of setuptools-option)
      :documentation
      "A list of names of Setuptools option and corresponding values
       that should be set during the build. Entries are of the form

         [ \"SECTION\", \"NAME\", \"VALUE\" ]

       where SECTION is the usually the name of a command like
       \"install\", NAME is an option within the section such as
       \"prefix\" within the \"install\" section and VALUE is the
       desired value of the option.")
     (command       :type string
      :documentation
      "The shell command that should be executed to perform the
       setuptools configuration and build."))
  "Configures a Python setuptools build step for the generated job.

   The ordering w.r.t. to other build steps is controlled via builder
   ordering constraints."
  (declare (ignore options))
  (push (constraint! (build ((:after dependency-download)))
          (shell (:command (wrapped-shell-command (:aspect.setuptools)
                             command))))
        (builders job)))

(defmethod lookup ((thing aspect-setuptools)
                   (name  (eql :aspect.setuptools.option-lines))
                   &key if-undefined)
  (declare (ignore if-undefined))
  (when-let ((options (value/cast thing :aspect.setuptools.options '())))
    (let+ (((&flet+ make-option ((section name value))
              (format nil "setopt -c \"~A\" -o \"~A\" -s \"~A\""
                      section name value))))
      (values (cons name `(:list ,@(map 'list #'make-option options))) '() t))))

;;; groovy script aspects

(define-aspect (groovy :job-var job) (builder-defining-mixin)
    (((kind :system) :type (or (eql :system) (eql :normal))
      :documentation
      "The execution environment of the build step.

       \"system\"

         The code is executed in the Jenkins master and can (assuming
         suitable permissions) manipulate all model objects.

       \"normal\"

         The code is executed in a forked JVM on the slave executing
         the surrounding build.")
     ((code (bail))  :type string
      :documentation
      "The groovy code to execute in the build step.")
     ((sandbox? nil) :type boolean
      :documentation
      "?"))
  "Configures a Groovy build step for the generated job.

   The ordering w.r.t. to other build steps is controlled via builder
   ordering constraints."
  (push (constraint! (build)
          (ecase kind
            (:system (system-groovy (:code code :sandbox? sandbox?)))
            (:normal (groovy        (:code code)))))
        (builders job)))
