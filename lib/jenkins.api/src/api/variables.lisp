;;;; variables.lisp --- Dynamic variables used by the api module.
;;;;
;;;; Copyright (C) 2012-2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.api)

(declaim (special *base-url* *username* *password*))

(defvar *base-url* "https://localhost:8080"
  "TODO")

(defvar *username* nil
  "TODO")

(defvar *password* nil
  "TODO")

(defparameter *cookie-jar* (make-instance 'drakma:cookie-jar))
