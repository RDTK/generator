;;;; protocol.lisp --- Protocol provided by the api module.
;;;;
;;;; Copyright (C) 2012, 2013, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.api)

;;; Generic model object protocol

(defgeneric id (object)
  (:documentation
   "TODO"))

(defgeneric commit! (object)
  (:documentation
   "Write transient changes to OBJECT back to the Jenkins server to
have them take effect and make them permanent."))

(defgeneric rename (object new-name)
  (:documentation
   "TODO(jmoringe): document"))

(defgeneric delete* (object)
  (:documentation
   "TODO(jmoringe): document"))

;;; Node protocol

#+no (defgeneric make-slave (name)
  (:documentation
   "TODO(jmoringe): document"))

(defgeneric online? (node)
  (:documentation
   "TODO"))

(defgeneric mark-online! (node
                          &key
                          if-online)
  (:documentation
   "TODO"))

(defgeneric mark-offline! (node
                           &key
                           if-offline)
  (:documentation
   "TODO"))

;;; Job protocol

#+no (defgeneric make-job (name)
  (:documentation
   "TODO(jmoringe): document"))

(defgeneric enable! (job)
  (:documentation
   "TODO"))

(defgeneric disable! (job)
  (:documentation
   "TODO"))

(defgeneric relate (parent child &key if-related)
  (:documentation
   "Add PARENT to the list of upstreams of CHILD.

    IF-RELATED controls the behavior in case PARENT is already in the
    list of upstreams of CHILD."))

(defgeneric unrelate (parent child &key if-not-related)
  (:documentation
   "Remove PARENT from the list of upstream of CHILD.

    IF-NOT-RELATED controls the behavior in case PARENT is not in the
    list of upstreams of CHILD."))
