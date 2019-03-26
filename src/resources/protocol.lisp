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

(defgeneric content (entry)
  (:documentation
   "Return the content of ENTRY a `nibbles:octet-vector'."))

(defgeneric info (entry)
  (:documentation
   "Return the info plist of ENTRY."))

;;; Group protocol

(defgeneric parent (group)
  (:documentation
   "Return the resource container containing GROUP."))

(defgeneric entries (group)
  (:documentation
   "Return a list of the entries of GROUP."))

(defgeneric find-entry (name group &key if-does-not-exist)
  (:documentation
   "Return entry named NAME in GROUP.

    IF-DOES-NOT-EXIST controls the behavior in case such an entry does
    not exist."))

(defgeneric (setf find-entry) (new-value name group &key if-does-not-exist)
  (:documentation
   "Store the entry NEW-VALUE under the NAME in GROUP.

    IF-DOES-NOT-EXIST is accepted for parity with `find-entry'."))

(defgeneric add-file (container file &key base-directory name info))

;;; Resources protocol

(defgeneric find-group (name container &key if-does-not-exist)
  (:documentation
   "Return the resource group named NAME in CONTAINER.

    IF-DOES-NOT-EXIST controls the behavior in case NAME does not name
    a resource group in CONTAINER."))

(defgeneric (setf find-group) (new-value name container &key if-does-not-exist)
  (:documentation
   "Store the resource group NEW-VALUE under NAME in CONTAINER.

    IF-DOES-NOT-EXIST is accepted for parity with `find-group'."))
