;;;; variables.lisp --- Variable provided by the model.variables module.
;;;;
;;;; Copyright (C) 2012-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.model.variables)

(defvar *variable-locations* (make-hash-table :weakness :value))

(defvar *traced-variables* '())
