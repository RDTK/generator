;;;; util.lisp --- Utilities used by the target.dockerfile module.
;;;;
;;;; Copyright (C) 2018, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:build-generator.deployment.dockerfile)

;;; Commands

(defun trim-command (command)
  (string-trim '(#\Space #\Tab #\Newline) command))
