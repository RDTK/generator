;;;; variables.lisp --- Variable provided by the model.variables module.
;;;;
;;;; Copyright (C) 2012-2017, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:build-generator.model.variables)

(defvar *variable-locations* (make-hash-table :weakness :value))

(defvar *traced-variables* '())
