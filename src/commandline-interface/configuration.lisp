;;;; configuration.lisp --- Configuration for the commandline-interface module.
;;;;
;;;; Copyright (C) 2013, 2014, 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.project.commandline-interface)

(configuration.options:define-schema *general-schema*
  "General configuration options."
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
  ("progress-style"   :type '(member :none :cmake)
                      :default :cmake
                      :documentation
                      "Progress display style.")
  ("non-interactive"  :type 'boolean :default nil
                      :documentation
                      "Avoid any user interaction.")
  ("num-processes"    :type 'positive-integer :default 8
                      ; :argument-name "NUMBER-OF-PROCESSES"
                      :documentation
                      "Number of processes to execute in parallel when checking out from repositories and analyzing working copies.")
  ("on-error"         :type '(member :abort :continue) :default :abort
                      ; :argument-name "POLICY"
                      :documentation
                      "Abort when encountering errors? Either \"abort\" or \"continue\".")
  ("cache-directory"  :type 'pathname
                      ; :argument-name "DIRECTORY"
                      :documentation
                      "Directory into which repository mirrors should be written.")
  ("temp-directory"   :type 'pathname :default #P"/tmp/"
                      ; :argument-name "DIRECTORY"
                      :documentation
                      "Directory into which temporary files should be written during analysis step.")
  ("report-directory" :type 'pathname
                      ; :argument-name "DIRECTORY"
                      :documentation
                      "Write information about distributions and projects into one or more report files. The written information includes most of the content of the respective underlying recipe but also expanded variable values, inferred variable values and analysis results.")
  ("dry-run"          :type 'boolean :default nil
                      :documentation
                      "Read recipes and perform the usual analysis but do not create or delete Jenkins jobs.")
  ("trace-variable"   :type '(list string :inherit? t)
                      ; :argument-name "VARIABLE-NAME"
                      :documentation
                      "Trace all accesses to the specified variable."))

(configuration.options:define-schema *jenkins-schema*
  "Options controlling communication with a Jenkins including
   authentication."
  ("base-uri"             :type    'string ; TODO puri:uri
                          :default "https://localhost:8080"
                          :documentation
                          "Jenkins base URI.")
  ("username"             :type    'string
                          :documentation
                          "Username for Jenkins authentication.")
  ("password"             :type    'string
                          :documentation
                          "Password for Jenkins authentication.")
  ("api-token"            :type    'string
                          :documentation
                          "API token for Jenkins authentication."))

(configuration.options:define-schema *generation-schema*
  "Options controlling the generation of Jenkins jobs."
  ("template-directory"   :type 'pathname
                          :documentation
                          "Directory containing sub-directories in turn containing template files. Must be used in combination with the mode option to select one of the sub-directories.")
  ("template"             :type '(list pathname :inherit? t)
                          :documentation
                          "Load one or more templates. Mutually exclusive with the mode option.") ;  This option can be supplied multiple times.
  ("distribution"         :type '(list pathname :inherit? t)
                          :documentation
                          "Load one or more distributions.") ; This option can be supplied multiple times.
  ("mode"                 :type 'string
                          :default "toolkit"
                          :documentation
                          "The mode according to which jobs should be generated. Selects a sub-directory of the directory specified using the template-directory option and thus a set of templates. Mutually exclusive with the template option.")
  ("set"                  :type '(list string :inherit? t) ; TODO list of key-value?
                           ; :short-name    "D"
                           ; :argument-name "VARIABLE-NAME=VALUE"
                           :documentation
                           "Overwrite a variable after loading the distribution. Arguments to this option have to be of the form VARIABLE-NAME=VALUE.")  ;  This option can be supplied multiple times.
  ("delete-other"         :type    'boolean
                          :documentation
                          "Delete previously automatically generated jobs when they are not re-created in this generation run.")
  ("delete-other-pattern" :type    'string ; TODO string/regular-expression
                          :default ".*"
                          ; :argument-name "REGEX"
                          :documentation
                          "When deleting previously automatically generated jobs, only consider jobs whose name matches the regular expression REGEX.

A common case, deleting only jobs belonging to the distribution being generated, can be achieved using the regular expression DISTRIBUTION-NAME$.")
  ("build-flow-fail"      :type     'boolean
                          :default  nil
                          :documentation
                          "Configure build-flow to fail when one of the jobs coordinated by it fails."))

(configuration.options:define-schema *schema*
  "Configuration options of the build generator."
  ("general"    *general-schema*)
  ("jenkins"    *jenkins-schema*)
  ("generation" *generation-schema*))
