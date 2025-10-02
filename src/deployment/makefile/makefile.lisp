;;;; makefile.lisp --- Helper classes and functions for writing Makefiles.
;;;;
;;;; Copyright (C) 2025 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:build-generator.deployment.makefile)

;;;

(defun write-rule (stream name &key dependencies directory command comment)
  (let* ((rule-name (util:safe-name name))
         (log-file  (format nil "~A.log" rule-name))
         (prefix    (string #\Tab)))
    ;; Write comment and rule head.
    (format stream "~@[# ~A~%~]~
                    ~A:~{ ~A~}~@
                    "
            comment rule-name (map 'list #'util:safe-name dependencies))
    ;; Write rule body (called "recipe" in the make documentation).
    (when command
      (let ((shell-string (escape-dollars (maybe-base64-encode command))))
        (pprint-logical-block (stream (list command) :per-line-prefix prefix)
          (format stream "@~
                          echo -en '\\e[1mExecuting ~A\\e[0m\\n'~@
                          +(~@
                            set -e~@
                            ~@[~
                              cd '~A'~@
                              export WORKSPACE=\"$$(pwd)\"~@
                            ~]~
                            ~@
                            ~A~@:_~
                          ) > '~A' 2>&1~@
                          if [ $$? -ne 0 ] ; then~@
                          ~2@Techo -en '\\e[35m'~@
                          ~2@Tcat '~:*~A'~@
                          ~2@Techo -en '\\e[0m'~@
                          ~2@Texit 1~@
                          fi~@
                          touch '~4:*~A'"
                  rule-name directory shell-string log-file)))
      (terpri stream))
    (terpri stream)))

;;;

(defclass rule (deploy:command-mixin
                print-items:print-items-mixin)
  ((%name         :initarg  :name
                  :type     string
                  :reader   name)
   (%dependencies :initarg  :dependencies
                  :type     list
                  :accessor dependencies
                  :initform '())
   (%directory    :initarg  :directory
                  :type     (or null string pathname)
                  :reader   directory
                  :initform nil)
   (%phony?       :initarg  :phony?
                  :type     boolean
                  :reader   phony?
                  :initform nil))
  (:default-initargs
   :name (more-conditions:missing-required-initarg 'rule :name)))

(defmethod print-items:print-items append ((object rule))
  `(((:name (:before :command)) "~A " ,(name object))))
