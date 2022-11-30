;;;; aspects-build.lisp --- Definitions of builder-creating aspects
;;;;
;;;; Copyright (C) 2012-2022 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:build-generator.model.aspects)

;;; Shell aspect

(define-aspect (shell :job-var job) (builder-defining-mixin)
    (((command (bail)) :type string
      :documentation
      "A fragment of shell code that should be executed as a build
       step at some point during the build."))
  "Adds a shell build step running COMMAND to the generated job.

   The ordering w.r.t. to other build steps is controlled via builder
   ordering constraints."
  (declare (ignore command))
  (let ((command (extend! aspect spec 'string :command)))
    (push (constraint! (build)
            (make-instance 'jenkins.api:builder/shell :command command))
          (jenkins.api:builders job))))

(defmethod extend! ((aspect aspect-shell)
                    (spec   t)
                    (output stream)
                    (target (eql :command)))
  (catch '%bail
    (destructuring-bind (command) (aspect-process-parameters aspect)
      (as-shell-command (output :aspect.shell) command))))

;;; Batch aspect

(define-aspect (batch :job-var job) (builder-defining-mixin)
    (((command (bail)) :type string
      :documentation
      "A fragment of Windows Batch doe that should be executed as a
       build step at some point during the build."))
  "Adds a Windows Batch build step running COMMAND to the generated job.

   The ordering w.r.t. to other build steps is controlled via builder
   ordering constraints."
  (push (constraint! (build)
          (make-instance 'jenkins.api:builder/batch :command command))
        (jenkins.api:builders job)))

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
  (declare (ignore command))
  (let ((command (extend! aspect spec 'string :command)))
    (push (constraint! (build ((:after dependency-download)))
            (make-instance 'jenkins.api:builder/shell :command command))
          (jenkins.api:builders job))))

(defmethod extend! ((aspect aspect-cmake/unix)
                    (spec   t)
                    (output stream)
                    (target (eql :command)))
  (destructuring-bind (command) (aspect-process-parameters aspect)
    (as-shell-command (output :aspect.cmake/unix) command)))

(var:define-variable :aspect.cmake/unix.find-commands list ; :write
  :documentation
  "Shell commands for finding upstream CMake packages.

   A list of strings of the form

     UPSTREAM_DIR=$(find ... -name UPSTREAMConfig.cmake ...)

   where UPSTREAM is the name of the respective upstream CMake
   module.")

(var:define-variable :aspect.cmake/unix.dir-options list ; :write
  :documentation
  "CMake commandline options for configuring upstream packages.

   A list of strings of the form

     UPSTREAM_DIR=${UPSTREAM_DIR}

   where UPSTREAM is the name of the respective upstream CMake
   module.")

(let+ (((&flet shellify (name)
          (make-variable/sh (string-upcase name))))
       ((&flet map-cmake-requirements (function aspect)
          (let* ((project-version   (model:parent (model:parent aspect)))
                 (dependencies      (list* project-version
                                           (model:dependencies project-version)))
                 (seen-requirements (make-hash-table :test #'equal)))
            (iter outer (for dependency in dependencies)
                  (for dependencies/alist
                       = (remove-if
                          (lambda (dependency)
                            (member (car dependency) '(nil :system)))
                          (uiop:symbol-call
                           '#:build-generator.model.project '#:direct-dependencies/reasons
                           dependency)))
                  (iter (for (nature target version) in (mappend #'cdr dependencies/alist))
                        (when (eq nature :cmake)
                          (unless (gethash target seen-requirements)
                            (setf (gethash target seen-requirements) t)
                            (in outer (collect (funcall function target))))))))))
       ((&flet result (name raw)
          (values (cons name (var:value-parse raw)) '() t))))

  (defmethod var:lookup ((thing aspect-cmake/unix)
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

  (defmethod var:lookup ((thing aspect-cmake/unix)
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
          (make-instance 'jenkins.api:builder/batch :command command))
        (jenkins.api:builders job)))

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
    (((properties           '()) :type (var:list-of string/name=value)
      :documentation
      "A list of Maven properties that should be set for the build.
       Entries are of the form NAME=VALUE.")
     (targets                    :type (var:list-of string)
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
          (make-instance 'jenkins.api:builder/maven
                         :properties          (mapcan (lambda (spec)
                                                        (let+ (((name value) (split-option spec)))
                                                          (list (make-keyword name) value)))
                                                      properties)
                         ;; hack to prevent useless progress output In
                         ;; the targets list because the maven plugin
                         ;; does not have specific fields for command
                         ;; line options
                         :targets             (list* "-B" targets)
                         :private-repository? private-repository?
                         :settings            (or settings-file :default)
                         :global-settings     (or global-settings-file :default)))
        (jenkins.api:builders job)))

(defmethod extend! ((aspect aspect-maven)
                    (spec   t)
                    (output stream)
                    (target (eql :command)))
  (destructuring-bind (properties targets &rest rest)
      (aspect-process-parameters aspect)
    (declare (ignore rest))
    (format output "mvn \\~%~
                      ~{~2@T-D~A~^ \\~%~}~
                      ~@[ \\~%~
                        ~2@T~{~A~^ ~}~
                      ~]"
            properties targets)))

;;; Setuptools aspect

(deftype setuptools-option ()
  '(cons string (cons string (cons string null))))

(defun every-setuptools-option (thing)
  (and (listp thing) (every (of-type 'setuptools-option) thing)))

(define-aspect (setuptools :job-var job) (builder-defining-mixin)
    (((options '()) :type (var:list-of setuptools-option)
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
  (declare (ignore options command))
  (let ((command (extend! aspect spec 'string :command)))
    (push (constraint! (build ((:after dependency-download)))
            (make-instance 'jenkins.api:builder/shell :command command))
         (jenkins.api:builders job))))

(defmethod extend! ((aspect aspect-setuptools)
                    (spec   t)
                    (output stream)
                    (target (eql :command)))
  (destructuring-bind (options command) (aspect-process-parameters aspect)
    (declare (ignore options))
    (as-shell-command (output :aspect.setuptools) command)))

(defmethod var:lookup ((thing aspect-setuptools)
                       (name  (eql :aspect.setuptools.option-lines))
                       &key if-undefined)
  (declare (ignore if-undefined))
  (when-let ((options (var:value/cast thing :aspect.setuptools.options '())))
    (let+ (((&flet+ make-option ((section name value))
              (format nil "setopt -c \"~A\" -o \"~A\" -s \"~A\""
                      section name value))))
      (values (cons name `(:list ,@(map 'list #'make-option options))) '() t))))

;;; groovy script aspects

(define-aspect (groovy :job-var job
                       :plugins ("groovy"))
    (builder-defining-mixin)
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
            (:system (make-instance 'jenkins.api:builder/system-groovy
                                    :code     code
                                    :sandbox? sandbox?))
            (:normal (make-instance 'jenkins.api:builder/groovy
                                    :code code))))
        (jenkins.api:builders job)))
