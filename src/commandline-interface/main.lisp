;;;; main.lisp --- Entry-point of commandline-interface module.
;;;;
;;;; Copyright (C) 2013, 2014, 2015, 2016, 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.project.commandline-interface)

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
