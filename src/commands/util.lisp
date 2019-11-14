;;;; util.lisp --- Utilities used in the commands module.
;;;;
;;;; Copyright (C) 2018, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:build-generator.commands)

;;; Password redaction

(defun sensitive-option? (option)
  (let ((name (last-elt (configuration.options:option-name option))))
    (find name '("password" "api-token") :test #'string=)))

(defun sensitive-commandline-option? (designator context)
  (when-let ((option (build-generator.commandline-options:option
                      (build-generator.commandline-options:find-option
                       designator context :if-does-not-exist nil))))
    (when (sensitive-option? option)
      (values t option))))

(defun maybe-redact-argument (argument previous context
                              &key (replacement "********"))
  (flet ((sensitive? (designator)
           (sensitive-commandline-option? designator context)))
    (cond ((and previous (sensitive? previous))
           replacement)
          ((when-let* ((index  (position #\= argument))
                       (prefix (subseq argument 0 index)))
             (when (sensitive? prefix)
               (format nil "~A=~A" prefix replacement))))
          (t
           argument))))

;;; Invocation description

(defun filtered-commandline-arguments (context &key (replacement "********"))
  (let ((arguments (uiop:command-line-arguments)))
    (loop :for previous = nil :then argument
          :for argument :in arguments
          :collect (maybe-redact-argument argument previous context
                                          :replacement replacement))))
