;;;; mixins.lisp --- Generic mixin classes used by project, templates, etc.
;;;;
;;;; Copyright (C) 2012, 2013, 2014, 2015, 2016 Jan Moringen
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
  (value-acons :name (name thing)
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
  ((implementations :initarg  :implementation
                    :type     list
                    :reader   implementations
                    :accessor %implementations
                    :initform nil
                    :documentation
                    ""))
  (:documentation
   "Instances of this class have associated implementation objects for
    which they are the specifications."))

(defmethod implementation ((specification specification-mixin))
  (assert (<= (length (implementations specification)) 1))
  (first (implementations specification)))

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
  (log:debug "~@<Checking whether to instantiate ~A with parent ~A.~@:>"
             spec parent)
  (let+ (((&flet value (name)
            (let ((key (make-keyword (string-upcase name))))
              (or (value parent key nil)
                  (value spec key nil)))))
         ((&labels matches? (regex value)
            (etypecase value
              (null   nil)
              (string (ppcre:scan-to-strings regex value))
              (cons   (some (curry #'matches? regex) value))))))
    (iter (for (expression . regex) in (conditions spec))
          (let* ((value    (value expression))
                 (matches? (matches? regex value)))
            (log:trace "~@<Checking ~S (=> ~S) against regex ~S: ~A.~@:>"
                       expression value regex matches?)
            (always matches?)))))

;;; `direct-variables-mixin'

(defvar *variable-locations* (make-hash-table :weakness :value))

(defclass direct-variables-mixin ()
  ((variables :initarg  :variables
              :type     list ; alist
              :accessor %direct-variables
              :initform '()
              :documentation
              "TODO as a plist "))
  (:documentation
   "TODO(jmoringe): document"))

(defmethod shared-initialize :around ((instance   direct-variables-mixin)
                                      (slot-names t)
                                      &key)
  (call-next-method)
  (loop :for cell :in (%direct-variables instance) :do
     (unless (gethash cell *variable-locations*)
       (setf (gethash cell *variable-locations*) instance))))

(defmethod direct-variables ((thing direct-variables-mixin))
  (append (%direct-variables thing)
          (when (next-method-p)
            (call-next-method))))

(defmethod variables append ((thing direct-variables-mixin))
  (copy-list (direct-variables thing)))

(defmethod (setf lookup) ((new-value t)
                          (thing     direct-variables-mixin)
                          (name      t)
                          &key
                          if-undefined)
  (declare (ignore if-undefined))
  (removef (%direct-variables thing) name :key #'car)
  (let ((cell (value-cons name new-value)))
    (push cell (%direct-variables thing))
    (setf (gethash cell *variable-locations*) thing)
    new-value))
