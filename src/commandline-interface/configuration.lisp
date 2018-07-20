;;;; configuration.lisp --- Configuration for the commandline-interface module.
;;;;
;;;; Copyright (C) 2013-2018 Jan Moringen
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
  ("on-error"         :type '(member :abort :continue) :default :continue
                      :documentation
                      "Abort when encountering errors? Either \"abort\" or \"continue\".")
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
  ;; Directories
  ("--cache-directory"      "cache-directory" "DIRECTORY")
  ("--temp-directory"       "temp-directory"  "DIRECTORY")
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
       synchronizer command local-arguments))

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
         ((&flet set-value (name value error-handler)
            (handler-bind ((error error-handler))
              (notify :added     name nil)
              (notify :new-value name value
                      :raw? (not (typep value 'boolean)))))))
    (jenkins.project.commandline-options:map-commandline-options
     (rcurry #'set-value #'error) "global" arguments
     :stop-at-positional? t)))
