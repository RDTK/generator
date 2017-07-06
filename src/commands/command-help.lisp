;;;; command-help.lisp --- Command for printing help.
;;;;
;;;; Copyright (C) 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.project.commands)

(defclass help ()
  ((command :initarg  :command
            :type     (or null string)
            :reader   command
            :initform nil
            :documentation
            "Nil or a command name for which to print help."))
  (:documentation
   "Print help either for all commands or for a given command."))

(service-provider:register-provider/class
 'command :help :class 'help)

(jenkins.project.commandline-options:define-option-mapping
    (*command-schema* "help")
  (0 "command" "COMMAND-NAME"))

(defmethod command-execute ((command help))
  (let+ ((stream       *standard-output*)
         (program-name "build-generator")
         ((&flet command-list ()
            (let* ((providers (service-provider:service-providers 'command))
                   (name      (compose #'string-downcase
                                       #'service-provider:provider-name))
                   (providers (sort (copy-list providers) #'string< :key name))
                   (width     (extremum (mapcar (compose #'length name) providers) #'>)))
              (mapcar (lambda (provider)
                        (list width
                              (funcall name provider)
                              (when-let ((string (documentation
                                                  provider t)))
                                (first-line-or-less string))))
                      providers))))
         ((&flet service-help ()
            (let ((options (jenkins.project.commandline-options:find-options
                            "global")))
              (format stream "Usage: ~A [GLOBAL-OPTIONS] COMMAND [COMMAND-OPTIONS]~@
                              ~@[~@
                                Global Options:~@
                                ~@
                                ~<  ~@:;~/jenkins.project.commandline-options:print-options/~@:>~@
                                ~@
                              ~@]~
                              Supported commands:~@
                              ~@
                              ~:{~2@T~VA  ~:[<not documented>~;~:*~A~]~%~}~
                              ~@
                              "
                      program-name (list options) (command-list)))))
         ((&flet command-help/provider (command-name provider)
            (let ((options  (jenkins.project.commandline-options:find-options
                             command-name)))
              (format stream "Usage: ~A [GLOBAL-OPTIONS] ~A~
                              ~/jenkins.project.commandline-options:print-usage/~@
                              ~@
                              ~:[~
                                <not documented>~
                              ~;~
                                ~:*~<~/jenkins.project.commandline-options::print-documentation/~:>~
                              ~]~
                              ~@
                              ~2:*~@[~
                              ~@
                              Options:~@
                                ~@
                                ~@<  ~@:;~/jenkins.project.commandline-options:print-options/~@:>~
                                ~@
                              ~@]~
                              ~@
                              "
                      program-name command-name options
                      (when-let ((documentation (documentation provider t)))
                        (list documentation))))))
         ((&flet command-help/name (command-name)
            (let* ((provider-name (make-keyword (string-upcase command-name)))
                   (provider      (service-provider:find-provider
                                   'command provider-name :if-does-not-exist nil)))
              (if provider
                  (command-help/provider command-name provider)
                  (format stream "\"~A\" is not a known command.~@
                                  ~@
                                  Supported commands:~@
                                  ~@
                                  ~:{~2@T~VA  ~:[<not documented>~;~:*~A~]~%~}~
                                  ~@
                                  "
                          command-name (command-list)))))))
    (if-let ((command-name (command command)))
      (command-help/name command-name)
      (service-help))))

;;; Utilities

(defun first-line-or-less (string &key (max 65))
  (let ((end (or (position #\Newline string)
                 (length string))))
    (subseq string 0 (min max end))))
