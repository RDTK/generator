;;;; package.lisp --- Package definition for the commandline-interface module.
;;;;
;;;; Copyright (C) 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.project.commandline-interface)

(defun update-synopsis ()
  "Create and return a commandline option tree."
  (clon:make-synopsis
   ;; Basic usage and specific options.
   :postfix "(INPUT-SPEC)+"
   :item    (clon:defgroup (:header "General Options")
              (flag   :long-name     "version"
                      :description
                      "Print version information and exit.")
              (flag   :long-name     "help"
                      :short-name    "h"
                      :description
                      "Print this help and exit.")
              (flag   :long-name     "swank"
                      :description
                      "Start a swank server.")
              (flag   :long-name     "debug"
                      :description
                      "Enable debug mode.")
              (enum   :long-name     "progress-style"
                      :enum          '(:cmake :vertical)
                      :default-value :vertical
                      :description
                      "Progress display style."))
   :item    (clon:defgroup (:header "Jenkins Options")
              (stropt :long-name     "template"
                      :short-name    "t"
                      :argument-name "TEMPLATE"
                      :description
                      "Load one or more templates. This option can be supplied multiple times.")
              (stropt :long-name     "distribution"
                      :short-name    "d"
                      :argument-name "DISTRIBUTION"
                      :description
                      "Load one or more distributions. This option can be supplied multiple times.")
              (stropt :long-name     "base-uri"
                      :short-name    "b"
                      :argument-name "URI"
                      :default-value "http://localhost:8080"
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
                      "Delete previously automatically generated jobs when they are not re-created in this generation run."))
   :item    (clon:defgroup (:header "Drupal Options")
              (stropt :long-name     "drupal-base-uri"
                      :argument-name "URI"
                      :default-value "https://toolkit.cit-ec.uni-bielefeld.de"
                      :description
                      "Drupal base URI.")
              (stropt :long-name     "drupal-username"
                      :description
                      "Username for Drupal authentication.")
              (stropt :long-name     "drupal-password"
                      :description
                      "Password for Drupal authentication."))))

(defun report-error (condition)
  (ignore-errors
   (format *error-output* "~&~A:~&~A~2%"
           (type-of condition) condition)))

(defun call-with-delayed-error-reporting (thunk
                                          &key
                                          debug?
                                          (report-function #'report-error))
  (let+ ((errors      '())
         (errors-lock (bt:make-lock))
         ((&flet collect-error (condition)
            (when debug?
              (terpri)
              (princ condition)
              (terpri)
              (sb-debug:print-backtrace))
            (bt:with-lock-held (errors-lock)
              (push condition errors))
            (continue))))
    ;; Execute THUNK collecting errors.
    (lparallel:task-handler-bind ((error #'collect-error))
      (handler-bind ((error #'collect-error))
        (funcall thunk)))

    ;; Report collected errors.
    (mapc (lambda (condition)
            (ignore-errors (funcall report-function condition)))
          errors)))

(defmacro with-delayed-error-reporting ((&key debug? report-function)
                                        &body body)
  `(call-with-delayed-error-reporting
    (lambda () ,@body)
    ,@(when debug? `(:debug? ,debug?))
    ,@(when report-function `(:report-function ,report-function))))

(defun main ()
  (log:config :thread :info)

  (update-synopsis)
  (clon:make-context)
  (when (clon:getopt :long-name "help")
    (clon:help)
    (uiop:quit))

  (with-delayed-error-reporting (:debug? (clon:getopt :long-name "debug"))
    (cerror "continue" "nothing here, yet")))
