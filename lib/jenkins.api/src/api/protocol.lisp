;;;; protocol.lisp --- Protocol provided by the api module.
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.api)


;;; Generic modle object protocol
;;

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
;;

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
;;

#+no (defgeneric make-job (name)
  (:documentation
   "TODO(jmoringe): document"))

(defgeneric enable! (job)
  (:documentation
   "TODO"))

(defgeneric disable! (job)
  (:documentation
   "TODO"))

(defgeneric relate (parent child)
  (:documentation
   "TODO"))

(defgeneric unrelate (parent child)
  (:documentation
   "TODO"))
