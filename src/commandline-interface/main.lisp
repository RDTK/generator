;;;; main.lisp --- Entry-point of commandline-interface module.
;;;;
;;;; Copyright (C) 2013-2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.project.commandline-interface)

(defun configure ()
  (when (and (interactive-stream-p *standard-output*)
             (interactive-stream-p *error-output*))
    (reinitialize-instance (configuration.options:find-option
                            '("global" "progress-style") *schema*)
                           :default :one-line))

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
                            (list section name) configuration)))
              (configuration.options:option-value
               option :if-does-not-exist nil)))))
    ;; Process configuration options.
    (handler-case
        (progn
          (configuration.options.sources:initialize source schema)
          (configuration.options.sources:process source synchronizer))
      (error (condition)
        (format t "Configuration error:~%~A~%" condition)
        (uiop:quit 3)))

    (let ((debug? (option-value "general" "debug")))

      (when debug?
        (describe configuration)
        (fresh-line))

      (when (or (emptyp (uiop:command-line-arguments))
                (option-value "general" "help"))
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

(defun make-error-policy (designator &key debug?)
  (let+ (((&flet continue/verbose (condition &key (debug? debug?))
            (unless (typep condition '(or undefined-function
                                          unbound-variable))
              (format *error-output* "~@<~A~@:>~2%" condition)
              (when debug?
                #+sbcl (sb-debug:print-backtrace))
              (continue condition))))
         ((&flet restart/condition (name &optional (condition? t) &rest args)
            (lambda (condition)
              (when-let ((restart (find-restart name condition)))
                (apply #'invoke-restart restart
                       (if condition?
                           (list* condition args)
                           args)))))))
    (case designator
      (:continue (lambda (condition)
                   (funcall (restart/condition 'jenkins.project.commands::defer
                                               t :debug? debug?)
                            condition)
                   (continue/verbose condition :debug? nil)))
      (t         (restart/condition 'abort nil)))))

(defun noting-serious-errors (error-policy function)
  (lambda (condition)
    (when (and (typep condition
                      '(and error
                            (not unfulfilled-project-dependency-error))))
      (funcall function condition))
    (funcall error-policy condition)))

(defun main ()
  (log:config :thread :warn)

  ;; Default to "one-line" progress style when apparently running
  ;; interactively.
  (when (and (interactive-stream-p *standard-output*)
             (interactive-stream-p *error-output*))
    (reinitialize-instance (configuration.options:find-option
                            '("global" "progress-style") *schema*)
                           :default :one-line))

  (let+ ((arguments (uiop:command-line-arguments))
         ((&flet execute-command-and-quit (code command &rest args)
            (jenkins.project.commands:command-execute
             (apply #'jenkins.project.commands:make-command
                    command args))
            (uiop:quit code)))
         (debugging? nil)
         ((&flet die (condition &optional usage? context)
            (format *error-output* "~@<~A~@:>~2%" condition)
            (when debugging?
              #+sbcl (sb-debug:print-backtrace))
            (if usage?
                (apply #'execute-command-and-quit
                       1 :help
                       (when (and context (not (equal context "global")))
                         (list :command context)))
                (uiop:quit 1))))
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
      (help?    (execute-command-and-quit 0 :help))
      (debug?   (setf debugging? t)))
    (handler-bind
        ((jenkins.project.commands:command-not-found-error
          (rcurry #'die t "global"))
         (jenkins.project.commands:command-configuration-problem
          (lambda (condition)
            (die condition t (jenkins.project.commands:command condition))))
         (error #'die)
         #+sbcl (sb-sys:interactive-interrupt #'die))
      (let* ((configuration   (configuration.options:sub-configuration
                               "commands.**" configuration))
             (command         (jenkins.project.commands:make-command
                               configuration))
             (serious-errors? nil))
        (jenkins.project.commands:execute-command
         command
         :num-processes   (option-value "global" "num-processes")
         :error-policy    (noting-serious-errors
                           (make-error-policy
                            (option-value "global" "on-error")
                            :debug? debugging?)
                           (lambda (condition)
                             (declare (ignore condition))
                             (setf serious-errors? t)))
         :progress-style  (option-value "global" "progress-style")
         :cache-directory (option-value "global" "cache-directory")
         :temp-directory  (option-value "global" "temp-directory"))
        (uiop:quit (if serious-errors? 1 0))))))

(eval-when (:load-toplevel)
  (check-variable-liveness))
