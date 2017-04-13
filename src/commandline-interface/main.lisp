;;;; main.lisp --- Entry-point of commandline-interface module.
;;;;
;;;; Copyright (C) 2013, 2014, 2015, 2016, 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.project.commandline-interface)

;;; Commandline options

(defun update-synopsis ()
  "Create and return a commandline option tree."
  (clon:make-synopsis
   ;; Basic usage and specific options.
   :item    (clon:defgroup (:header "General Options")
              (flag    :long-name     "version"
                       :description
                       "Print version information and exit.")
              (flag    :long-name     "help"
                       :short-name    "h"
                       :description
                       "Print this help and exit.")
              (flag    :long-name     "swank"
                       :description
                       "Start a swank server.")
              (flag    :long-name     "debug"
                       :description
                       "Enable debug mode.")
              (enum    :long-name     "progress-style"
                       :enum          '(:none :cmake :one-line)
                       :description
                       "Progress display style.")
              (flag    :long-name    "non-interactive"
                       :description
                       "Avoid any user interaction.")
              (lispobj :long-name    "num-processes"
                       :short-name   "j"
                       :typespec     'positive-integer
                       :argument-name "NUMBER-OF-PROCESSES"
                       :description
                       "Number of processes to execute in parallel when checking out from repositories and analyzing working copies.")
              (enum    :long-name     "on-error"
                       :enum          '(:abort :continue)
                       :argument-name "POLICY"
                       :description
                       "Abort when encountering errors? Either \"abort\" or \"continue\".")
              (path    :long-name    "cache-directory"
                       :type         :directory
                       :argument-name "DIRECTORY"
                       :description
                       "Directory into which repository mirrors should be written.")
              (path    :long-name    "temp-directory"
                       :type         :directory
                       :argument-name "DIRECTORY"
                       :description
                       "Directory into which temporary files should be written during analysis step.")
              (path    :long-name    "report-directory"
                       :type         :directory
                       :argument-name "DIRECTORY"
                       :description
                       "Write information about distributions and projects into one or more report files. The written information includes most of the content of the respective underlying recipe but also expanded variable values, inferred variable values and analysis results.")
              (flag    :long-name    "dry-run"
                       :description
                       "Read recipes and perform the usual analysis but do not create or delete Jenkins jobs.")
              (stropt  :long-name    "trace-variable"
                       :argument-name "VARIABLE-NAME"
                       :description
                       "Trace all accesses to the specified variable.")
              (flag    :long-name     "info-variables"
                       :description
                       "Show information about variables.")
              (flag    :long-name     "info-aspects"
                       :description
                       "Show information about available aspects."))

   :item    (clon:defgroup (:header "Processing Options")
              (path   :long-name     "template-directory"
                      :type          :directory
                      :argument-name "DIRECTORY"
                      :description
                      "Directory containing sub-directories in turn containing template files. Must be used in combination with the mode option to select one of the sub-directories.")
              (stropt :long-name     "template"
                      :short-name    "t"
                      :argument-name "TEMPLATE"
                      :description
                      "Load one or more templates. This option can be supplied multiple times. Mutually exclusive with the mode option.")
              (stropt :long-name     "distribution"
                      :short-name    "d"
                      :argument-name "DISTRIBUTION"
                      :description
                      "Load one or more distributions. This option can be supplied multiple times.")
              (stropt :long-name     "mode"
                      :short-name    "m"
                      :argument-name "MODE"
                      :description
                      "The mode according to which jobs should be generated. Selects a sub-directory of the directory specified using the template-directory option and thus a set of templates. Mutually exclusive with the template option.")
              (stropt :long-name     "set"
                      :short-name    "D"
                      :argument-name "VARIABLE-NAME=VALUE"
                      :description
                      "Overwrite a variable after loading the distribution. Arguments to this option have to be of the form VARIABLE-NAME=VALUE. This option can be supplied multiple times."))

   :item    (clon:defgroup (:header "Jenkins Options")
              (stropt :long-name     "base-uri"
                      :short-name    "b"
                      :argument-name "URI"
                      :description
                      "Jenkins base URI.")
              (stropt :long-name     "username"
                      :short-name    "u"
                      :description
                      "Username for Jenkins authentication.")
              (stropt :long-name     "password"
                      :short-name    "p"
                      :description
                      "Password for Jenkins authentication.")
              (stropt :long-name     "api-token"
                      :short-name    "a"
                      :description
                      "API token for Jenkins authentication.")
              (flag   :long-name     "delete-other"
                      :description
                      "Delete previously automatically generated jobs when they are not re-created in this generation run.")
              (stropt :long-name     "delete-other-pattern"
                      :argument-name "REGEX"
                      :description
                      "When deleting previously automatically generated jobs, only consider jobs whose name matches the regular expression REGEX.

A common case, deleting only jobs belonging to the distribution being generated, can be achieved using the regular expression DISTRIBUTION-NAME$."))))

(defun configure ()
  (let+ ((*print-right-margin*   (if-let ((value (sb-posix:getenv "COLUMNS"))) ; TODO
                                   (parse-integer value)
                                   100))
         (schema        *schema*)
         (configuration (configuration.options:make-configuration schema))
         (source        (configuration.options.sources:make-source
                         :common-cascade ; TODO environment variables
                         :basename "build-generator"
                         :syntax   :ini))
         (synchronizer  (make-instance 'configuration.options:standard-synchronizer
                                       :target configuration))
         ((&flet option-value (section name)
            (let+ ((option (configuration.options:find-option
                            (list section name) configuration))
                   ((&values value source)
                    (if (typep (configuration.options:option-type option)
                               '(cons (eql list)))
                        (iter (for spec next (clon:getopt :long-name name))
                              (while spec)
                              (collect spec :into values)
                              (finally (when values (return (values values t)))))
                        (clon:getopt :long-name name))))
              (if source
                  (values value :commandline)
                  (configuration.options:option-value
                   option :if-does-not-exist nil))))))
    ;; Process configuration options.
    (handler-case
        (progn
          (configuration.options.sources:initialize source schema)
          (configuration.options.sources:process source synchronizer))
      (error (condition)
        (format t "Configuration error:~%~A~%" condition)
        (uiop:quit 3)))

    ;; Process commandline options.
    (update-synopsis)
    (clon:make-context)

    (let ((debug? (option-value "general" "debug")))

      (when debug?
        (describe configuration)
        (fresh-line))

      (when (or (emptyp (uiop:command-line-arguments))
                (option-value "general" "help"))
        (clon:help)
        (uiop:quit))

      (when (option-value "general" "version")
        (format *standard-output* "~A version ~A~&"
                "build-generator" *generator-version*)
        (uiop:quit))

      (when (option-value "general" "info-variables")
        (execute-command "info-variables" '())
        (uiop:quit))

      (when (option-value "general" "info-aspects")
        (execute-command "info-aspects" '())
        (uiop:quit))

      (values #'option-value configuration debug?))))

(defun make-error-policy (designator)
  (let+ (((&flet continue/verbose (condition)
            (format *error-output* "~@<~A~@:>~2%" condition)
            (continue condition)))
         ((&flet restart/condition (name &optional (condition? t))
            (lambda (condition)
              (when-let ((restart (find-restart name condition)))
                (apply #'invoke-restart restart
                       (when condition? (list condition)))))))
         (non-dependency-errors? nil)
         (error-policy           (case designator
                                   (:continue (lambda (condition)
                                                (funcall (restart/condition 'jenkins.project.commands::defer) condition)
                                                (continue/verbose condition)))
                                   (t         (restart/condition 'abort nil)))))
    (lambda (condition)
      (when (typep condition
                   '(and error
                     (not unfulfilled-project-dependency-error)))
        (setf non-dependency-errors? t))
      (funcall error-policy condition)
      #+no (cond
        ((and (typep condition 'simple-phase-error)
              (funcall error-policy condition)
              nil))
        ((funcall (restart/condition 'jenkins.project.commands::defer) condition))
        ((funcall (restart/condition 'abort) condition))))))

(defun main ()
  (log:config :thread :warn)
  (let+ ((arguments (uiop:command-line-arguments))
         ((&flet execute-command-and-quit (code command &rest args)
            (jenkins.project.commands:command-execute
             (apply #'jenkins.project.commands:make-command
                    command args))
            (uiop:quit code)))
         ((&flet die (condition &optional usage? context)
            (format *error-output* "~@<~A~@:>~2%" condition)
            (when usage?
              (apply #'execute-command-and-quit
                     1 :help (when (and context (not (equal context "global")))
                               (list :command context))))))
         ((&values option-value &ign configuration &ign
                   (&plist-r/o
                    (version? :version?) (help? :help?) (debug? :debug?)))
          (handler-bind (((and error jenkins.project.commandline-options:option-context-condition)
                          (lambda (condition)
                            (die condition t (jenkins.project.commandline-options:context condition))))
                         (error (rcurry #'die t "global")))
            (process-configuration-and-commandline-arguments arguments))) ; TODO this calls configure-command but reported conditions are not right. e.g. report -D foo.distribution produces "The "-D" option requires a VARIABLE-NAME=VALUE argument." and the generic help. does not mention the command and does not print the command-specific help
         ((&flet option-value (&rest args)
            (apply option-value args))))
    (cond
      (version? (execute-command-and-quit 0 :version))
      (help?    (execute-command-and-quit 0 :help)))
    (handler-bind
        ((jenkins.project.commands:command-not-found-error
          (rcurry #'die t "global"))
         (jenkins.project.commands:command-configuration-problem
          (lambda (condition)
            (die condition t (jenkins.project.commands:command condition))))
         (error #'die))
      (let* ((configuration (configuration.options:sub-configuration
                             "commands.**" configuration))
             (command       (jenkins.project.commands:make-command
                             configuration)))
        (jenkins.project.commands:execute-command
         command
         :num-processes  (option-value "global" "num-processes")
         :error-policy   (make-error-policy
                          (option-value "global" "on-error"))
         :progress-style (option-value "global" "progress-style"))))
    (uiop:quit)))

(eval-when (:load-toplevel)
  (check-variable-liveness))
