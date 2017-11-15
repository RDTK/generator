;;;; mixins.lisp --- Mixin classes used in the model.project module.
;;;;
;;;; Copyright (C) 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.model.project)

(defclass person-container-mixin ()
  ((persons :initarg  :persons
            :reader   persons-in-roles/plist
            :initform '()
            :documentation
            "Plist of the form

               (ROLE1 PERSON-LIST1 ...)"))
  (:documentation
   "Adds to classes a plist of roles and person lists."))

(defmethod persons ((container person-container-mixin))
  (loop :for (role persons) :on (persons-in-roles/plist container) :by #'cddr
     :appending persons))

(defmethod persons-in-role ((role symbol) (container person-container-mixin))
  (getf (persons-in-roles/plist container) role))
