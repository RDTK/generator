;;;; functions-version.lisp --- Functions related to the program version.
;;;;
;;;; Copyright (C) 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.project.commands)

(defparameter *generator-version*
  (asdf:component-version (asdf:find-system :jenkins.project)))

(defun generator-version ()
  *generator-version*)
