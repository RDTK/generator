;;;; locations.lisp --- Source locations for recipe concrete syntax.
;;;;
;;;; Copyright (C) 2016-2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.model.project)

(defvar *locations* (make-hash-table :test #'eq))

(defvar *location->object* (make-hash-table :test #'eq))

(defvar *locations-lock* (bt:make-lock "locations"))

(defun object-at (location)
  (bt:with-lock-held (*locations-lock*)
    (gethash location *location->object*)))

(defun location-of (object)
  (bt:with-lock-held (*locations-lock*)
    (gethash object *locations*)))

(defun (setf location-of) (new-value object)
  (bt:with-lock-held (*locations-lock*)
    (setf (gethash object    *locations*)        new-value
          (gethash new-value *location->object*) object)))

(defun copy-location (old-object new-object)
  (setf (location-of new-object) (location-of old-object))
  new-object)
