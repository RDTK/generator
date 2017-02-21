;;;; variables.lisp --- Variables used in the analysis module.
;;;;
;;;; Copyright (C) 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.analysis)

(defvar *cache-version*
  (asdf:component-version (asdf:find-system :jenkins.project)))
