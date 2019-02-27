;;;; configuration.lisp --- Configuration for the commandline-interface module.
;;;;
;;;; Copyright (C) 2013-2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.project.commandline-interface)

;;; Schema

(configuration.options:define-schema *global-schema*
  "Global configuration options."
  ;; Generic
  ("version"          :type 'boolean :default nil
                      :documentation
                      "Print version information and exit.")
  ("help"             :type 'boolean :default nil
                      :documentation
                      "Print this help and exit.")
  ("swank"            :type 'boolean :default nil
                      :documentation
                      "Start a swank server.")
  ("debug"            :type 'boolean :default nil
                      :documentation
                      "Enable debug mode.")
  ;; Execution mode and feedback
  ("on-error"         :type 'error-policy
                      :default '((caused-by-unfulfilled-project-dependency-error . :continue)
                                 (t                                              . :fail))
                      :documentation
                      #.(format nil "Continue when encountering errors?~@
                         ~@
                         Can be simply~@
                         ~2@T\"abort\"    to abort immediately for any ~
                                          error~@
                         ~2@T\"fail\"     to continue but indicate failure ~
                                          for all errors~@
                         ~2@T\"continue\" to continue without ~
                                          indicating failure for all ~
                                          errors~@

                         ~2@T\"debug\"    to enter the debug for all ~
                                          errors~@
                         ~@
                         To choose specific actions for particular ~@
                         errors, rules can be written according to the ~@
                         following grammar:~@
                         ~2@Terror-policy ::= rule* default~@
                         ~2@Trule         ::= error \"=>\" action \":\"~@
                         ~2@Terror        ::= ~{\"~A\"~^ | ~}~@
                         ~2@Tdefault      ::= action~@
                         ~2@Taction       ::= ~{\"~(~A~)\"~^ | ~}~@
                         ~@
                         Example:~@
                         ~@
                         ~2@Tdependency-error=>continue:analysis-error=>fail:abort~@
                         ~@
                         The above continues the run with exit code ~
                         zero in case dependency-errors are ~
                         encountered, continues and returns a non-zero ~
                         exit code for analysis-errors and immediately ~
                         aborts with non-zero exit code for all other ~
                         errors."
                                (map 'list (lambda+ ((name . alias))
                                             (or alias (string-downcase name)))
                                     *condition-types*)
                                *error-handling-actions*))
  ("non-interactive"  :type 'boolean :default nil
                      :documentation
                      "Avoid any user interaction.")
  ("num-processes"    :type 'positive-integer :default 8
                      :documentation
                      "Number of threads (and processes) to execute in parallel.")
  ("progress-style"   :type '(member :none :cmake :one-line)
                      :default :cmake
                      :documentation
                      "Progress display style.")
  ;; Directories
  ("cache-directory"  :type 'configuration.options:directory-pathname
                      :documentation
                      "Directory into which cached data like repository mirrors should be written.")
  ("temp-directory"   :type 'configuration.options:directory-pathname
                      :default #P"/tmp/"
                      :documentation
                      "Directory into which temporary files should be written.")
  ("cache-age-limit"  :type '(or null non-negative-integer)
                      :default 1800
                      :documentation
                      #.(format nil "Acceptable age of cached ~
                         information in seconds.~@
                         ~@
                         Older cached information will not be used and ~
                         will be replaced by newly computed ~
                         information."))
  ;; Application-level debugging
  ("trace-variable"   :type '(list string :inherit? t)
                      :documentation
                      "Trace all accesses to the specified variable."))

(configuration.options:define-schema *schema*
  "Configuration options of the build generator."
  ("global"   *global-schema*)
  ("commands" jenkins.project.commands::*command-schema*))

;;; Commandline options

(jenkins.project.commandline-options:define-option-mapping (*schema* "global")
  ;; Meta
  ("--version"              "version")
  (("-h" "--help")          "help")
  ("--debug"                "debug")
  ;; Execution mode and feedback
  ("--on-error"             "on-error"        "POLICY")
  ("--non-interactive"      "non-interactive")
  (("-j" "--num-processes") "num-processes"   "NUMBER-OF-PROCESSES")
  ("--progress-style"       "progress-style"  "STYLE")
  ;; Directories and cache
  ("--temp-directory"       "temp-directory"  "DIRECTORY")
  ("--cache-directory"      "cache-directory" "DIRECTORY")
  ("--cache-age-limit"      "cache-age-limit" "AGE-IN-SECONDS")
  ;; Application-level debugging
  ("--trace-variable"       "trace-variable"  "VARIABLE-NAME"))

;;; Configuration processing

(defun process-configuration-and-commandline-arguments (arguments)
  "Load configuration from sources then add ARGUMENTS."

  (let+ ((config-debug? (configuration.options.debug:maybe-enable-debugging
                         "BUILD_GENERATOR_"))
         (schema        *schema*)
         (configuration (configuration.options:make-configuration schema))
         (source        (configuration.options.sources:make-source
                         :common-cascade
                         :basename "build-generator"
                         :syntax   :ini))
         (synchronizer  (make-instance 'configuration.options:standard-synchronizer
                                       :target configuration))
         ((&flet option-value (&rest components)
            (let ((option (configuration.options:find-option
                           components configuration)))
              (configuration.options:option-value
               option :if-does-not-exist nil)))))
    ;; Process configuration sources other than commandline arguments.
    (configuration.options.sources:initialize source schema)
    (configuration.options.sources:process source synchronizer)

    ;; Process global commandline options and split ARGUMENTS into
    ;;
    ;;   GLOBAL-ARGUMENTS COMMAND LOCAL-ARGUMENTS
    ;;
    (let* ((command-index   (process-global-commandline-arguments
                             synchronizer arguments))
           (command         (if command-index
                                (elt arguments command-index)
                                "help"))
           (local-arguments (when command-index
                              (subseq arguments (1+ command-index)))))

      ;; Process local (i.e. consumed by command) commandline options.
      (jenkins.project.commands:configure-command
       synchronizer command local-arguments)
      ;; If the help command will be executed because no command has
      ;; been supplied, enable brief output.
      (unless command-index
        (setf (configuration.options:option-value
               (configuration.options:find-option
                '("commands" "help" "brief?") configuration))
              t)))

    (when config-debug?
      (configuration.options.debug:output
       (with-output-to-string (stream)
         (describe configuration stream)
         (terpri stream))))

    ;; Extract and interpret "special" options.
    (let+ (((version? help? debug?)
            (map 'list (curry #'option-value "global")
                 '("version" "help" "debug"))))

      (when debug? (log:config :debug))

      (values #'option-value schema configuration synchronizer
              (list :version? version? :help? help? :debug? debug?)))))

(defun process-global-commandline-arguments (synchronizer arguments)
  (let+ (((&flet notify (name event value &key (raw? t))
            (configuration.options:notify
             synchronizer name event value :source :commandline :raw? raw?)))
         ((&flet set-value (name value)
            (notify :added     name nil)
            (notify :new-value name value
                    :raw? (not (typep value 'boolean))))))
    (jenkins.project.commandline-options:map-commandline-options
     #'set-value "global" arguments :stop-at-positional? t)))

(defun (setf default-progress-style) (new-value)
  (reinitialize-instance (configuration.options:find-option
                          '("global" "progress-style") *schema*)
                         :default new-value))

(defun choose-default-progress-style (&key
                                      (standard-output *standard-output*)
                                      (error-output    *error-output*)
                                      (terminal-type   (uiop:getenv "TERM")))
  ;; Default to "one-line" progress style when apparently running
  ;; interactively.
  (when (and (interactive-stream-p standard-output)
             (interactive-stream-p error-output)
             (not (equal terminal-type "dumb")))
    (setf (default-progress-style) :one-line)))
