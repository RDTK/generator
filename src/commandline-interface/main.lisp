;;;; main.lisp --- Entry-point of commandline-interface module.
;;;;
;;;; Copyright (C) 2013-2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.project.commandline-interface)

(defun make-error-policy (designator &key debug?)
  (let+ (((&flet continue/verbose (condition &key (debug? debug?))
            (when (typep condition 'jenkins.util:continuable-error)
              (format *error-output* "~@<~A~@:>~2%" condition)
              (when debug?
                #+sbcl (sb-debug:print-backtrace))
              (when-let ((restart (jenkins.util:find-continue-restart condition)))
                (invoke-restart restart)))))
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
                            (not jenkins.project.commands::unfulfilled-project-dependency-error))))
      (funcall function condition))
    (funcall error-policy condition)))

(defun main ()
  (log:config :thread :warn)
  (choose-default-progress-style)

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
                       1 :help :brief? t
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
    (cond (version? (execute-command-and-quit 0 :version))
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
         :temp-directory  (option-value "global" "temp-directory")
         :trace-variables (option-value "global" "trace-variable"))
        (uiop:quit (if serious-errors? 1 0))))))

(eval-when (:load-toplevel)
  (check-variable-liveness))
