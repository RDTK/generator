;;;; mixins.lisp --- Generic mixin classes used by project, templates, etc.
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.project)

;;; `named-mixin'

(defclass named-mixin (print-items-mixin)
  ((name :initarg  :name
         :type     string
         :reader   name
         :documentation
         ""))
  (:documentation
   "TODO(jmoringe): document"))

(defmethod direct-variables ((thing named-mixin))
  (append (list :name (name thing))
          (when (next-method-p)
            (call-next-method))))

(defmethod print-items append ((object named-mixin))
  (let+ (((&labels ancestor-names (object)
            (append (when (compute-applicable-methods #'parent (list object))
                      (when-let ((parent (parent object)))
                        (ancestor-names parent)))
                    (list (let ((name (name object)))
                            (if (emptyp name)
                                "<empty>"
                                name)))))))
    `((:name ,(ancestor-names object) "~{~A~^:~}"))))

;;; `parented-mixin'

(defclass parented-mixin ()
  ((parent :initarg  :parent
           :reader   parent
           :initform nil
           :documentation
           ""))
  (:documentation
   "TODO(jmoringe): document"))

(defmethod variables append ((thing parented-mixin))
  (when-let ((parent (parent thing)))
    (variables parent)))

;;; `implementation-mixin'

(defclass implementation-mixin ()
  ((specification :initarg  :specification
                  :reader   specification
                  :accessor %specification
                  :initform nil
                  :documentation
                  ""))
  (:documentation
   "Instances of this class may have associated specification objects
    of which they are implementations."))

;;; `specification-mixin'

(defclass specification-mixin ()
  ((implementation :initarg  :implementation
                   :reader   implementation
                   :accessor %implementation
                   :initform nil
                   :documentation
                   ""))
  (:documentation
   "Instances of this class have associated implementation objects for
    which they are the specifications."))

;;; `conditional-mixin'

(defclass conditional-mixin ()
  ((conditions :initarg  :conditions
               :type     list
               :reader   conditions
               :initform '()
               :documentation
               ""))
  (:documentation
   "TODO(jmoringe): document"))

(defmethod instantiate? ((spec conditional-mixin) (parent t))
  (let+ (((&flet value (name)
            (let ((key (make-keyword (string-upcase name))))
              (or (handler-case
                      (value parent key)
                    (error () nil))
                  (handler-case
                      (value spec key)
                    (error () nil)))))))
    (iter (for (expression regex) on (conditions spec) :by #'cddr)
          (always (when-let ((value (value expression)))
                    (ppcre:scan regex value))))))

;;; `direct-variables-mixin'

(defclass direct-variables-mixin ()
  ((variables :initarg  :variables
              :type     list ; plist
              :accessor %direct-variables
              :initform '()
              :documentation
              "TODO as a plist "))
  (:documentation
   "TODO(jmoringe): document"))

(defmethod direct-variables ((thing direct-variables-mixin))
  (append (%direct-variables thing)
          (when (next-method-p)
            (call-next-method))))

(defmethod variables append ((thing direct-variables-mixin))
  (copy-list (direct-variables thing)))

(defmethod (setf lookup) ((new-value t)
                          (thing     direct-variables-mixin)
                          (name      t))
  (setf (getf (%direct-variables thing) name) new-value))
