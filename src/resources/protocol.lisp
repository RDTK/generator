;;;; protocol.lisp --- Protocol provided by the resources module.
;;;;
;;;; Copyright (C) 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.project.resources)

;;; Name protocol

(defgeneric name (thing)
  (:documentation
   "Return the name of THING."))

;;; Size protocol

(defgeneric octet-count (container)
  (:documentation
   "Return the number of octets stored in CONTAINER."))

;;; Entry protocol

(defgeneric content (entry))

(defgeneric info (entry))

;;; Group protocol

(defgeneric parent (group))

(defgeneric entries (group))

(defgeneric find-entry (name group &key if-does-not-exist))

(defgeneric (setf find-entry) (new-value name group))

(defgeneric add-file (container file &key base-directory name info))

;;; Resources protocol

(defgeneric find-group (name container &key if-does-not-exist)
  (:documentation
   "Return the resource group named NAME in CONTAINER."))

(defgeneric (setf find-group) (new-value name container &key if-does-not-exist)
  (:documentation
   "TODO"))
