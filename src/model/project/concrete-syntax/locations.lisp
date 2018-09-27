;;;; locations.lisp --- Source locations for recipe concrete syntax.
;;;;
;;;; Copyright (C) 2016-2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.model.project)

(declaim (special *locations*)
         (type locations *locations*))

(defclass locations ()
  ((%parent           :initarg  :parent
                      :reader   parent
                      :initform *locations*)
   (%object->location :reader   object->location
                      :initform (make-hash-table :test #'eq))
   (%location->object :reader   location->object
                      :initform (make-hash-table :test #'eq))
   (%hook             :initarg  :hook
                      :reader   %hook
                      :initform :inherit)))

(defvar *locations* (make-instance 'locations :parent nil))

(defvar *locations-lock* (bt:make-lock "locations"))

(defun object-at (location &optional (repository *locations*))
  (or (bt:with-lock-held (*locations-lock*)
        (gethash location (location->object repository)))
      (when-let ((parent (parent repository)))
        (object-at location parent))))

(defun location-of (object &optional (repository *locations*))
  (or (bt:with-lock-held (*locations-lock*)
        (gethash object (object->location repository)))
      (when-let ((parent (parent repository)))
        (location-of object repository))))

(defun (setf location-of) (new-value object &optional (repository *locations*))
  (bt:with-lock-held (*locations-lock*)
    (setf (gethash object    (object->location repository)) new-value
          (gethash new-value (location->object repository)) object)
    (when-let ((hook (hook repository)))
      (funcall hook object new-value))))

(defun copy-location (old-object new-object)
  (setf (location-of new-object) (location-of old-object))
  new-object)

(defun hook (repository)
  (let ((hook (%hook repository)))
    (case hook
      ((nil)    nil)
      (:inherit (when-let ((parent (parent repository)))
                  (hook parent)))
      (t        hook))))
