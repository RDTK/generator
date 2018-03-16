;;;; protocol.lisp --- Protocol provided by the commands module.
;;;;
;;;; Copyright (C) 2017, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.project.commands)

;;; Command service

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass command-service (service-provider:standard-service
                             service-provider::change-hook-mixin)
    ()))

(service-provider:define-service command
  (:service-class command-service))

;;; Command protocol

(defgeneric command-execute (command)
  (:documentation
   "Execute the already-configured COMMAND."))

(defgeneric make-command (provider &rest args)
  (:method ((provider t) &rest args)
    (apply #'service-provider:make-provider 'command provider args))
  (:documentation
   "Make and return a command according to PROVIDER and ARGS."))

;;; Command configuration

(defvar *command-schema*
  (configuration.options.service-provider:service-schema
   (service-provider:find-service 'command)))

;;; High-level interface: find, instantiate and execute a command

(defun configure-command (synchronizer name arguments)
  "Configure the command designate by NAME with ARGUMENTS.

   SYNCHRONIZER is responsible for putting the indicated option values
   into a configuration container."
  (let+ ((prefix (configuration.options:make-name "commands"))
         ((&flet notify (name event value &key (raw? t))
            (configuration.options:notify
             synchronizer name event value :source :commandline :raw? raw?)))
         ((&flet set-value (name value error-handler)
            (let ((name (configuration.options:merge-names
                         prefix name)))
              (handler-bind ((error error-handler))
                (notify :added     name nil)
                (notify :new-value name value
                        :raw? (not (typep value 'boolean))))))))

    ;; Select provider according to NAME.
    (set-value '("provider") name
               (lambda (condition)
                 (declare (ignore condition))
                 (error 'command-not-found-error :command name)))

    ;; Configure provider according to ARGUMENTS.
    (jenkins.project.commandline-options:map-commandline-options
     (lambda (option-path value)
       (set-value option-path value
                  (lambda (condition)
                    (error 'option-value-error
                           :context name
                           :option  (second option-path)
                           :command name
                           :value   value
                           :cause   condition))))
     name arguments)))

(defvar *cache-directory*)
(defvar *temp-directory*)

(defun execute-command (command
                        &key
                        (num-processes  1)
                        (error-policy   #'error)
                        (progress-style :cmake)
                        cache-directory
                        temp-directory)
  (let ((main-thread (bt:current-thread))
        (lock        (bt:make-lock)))
    (setf lparallel:*kernel* (lparallel:make-kernel num-processes))
    (unwind-protect-case ()
         (handler-bind ((error error-policy)
                        (more-conditions:progress-condition
                         (make-progress-handler progress-style)))
           (lparallel:task-handler-bind
               ((error error-policy)
                (more-conditions:progress-condition
                 (lambda (condition)
                   (bt:interrupt-thread
                    main-thread (lambda ()
                                  (sb-sys:without-interrupts
                                    (bt:with-lock-held (lock)
                                      (signal condition))))))))
             (let ((*cache-directory* cache-directory)
                   (*temp-directory*  temp-directory))
               (command-execute command))))
      (:normal
       (lparallel:end-kernel :wait t))
      (:abort
       (lparallel:end-kernel :wait nil)
       (lparallel:kill-tasks :default)))))

(defun make-progress-handler (style)
  (lambda (condition)
    (case style
      (:none)
      (:cmake
       (princ condition)
       (fresh-line))
      (:one-line
       (let* ((progress      (progress-condition-progress condition))
              (progress/real (progress->real progress))
              (width    20))
         (format t "~C[2K[~VA] ~A~C[G"
                 #\Escape
                 width
                 (make-string (floor progress/real (/ width))
                              :initial-element #\#)
                 condition
                 #\Escape)
         (if (eq progress t)
             (terpri)
             (force-output)))))))
