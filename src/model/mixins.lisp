;;;; mixins.lisp --- Generic mixin classes used by project, templates, etc.
;;;;
;;;; Copyright (C) 2012-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.model)

;;; `named-mixin'

(defclass named-mixin (print-items:print-items-mixin)
  ((name :initarg  :name
         :type     string
         :reader   name
         :documentation
         ""))
  (:documentation
   "Intended to be mixed into classes representing named model objects."))

(defmethod direct-variables ((thing named-mixin))
  (value-acons :name (name thing)
               (when (next-method-p)
                 (call-next-method))))

(defmethod print-items:print-items append ((object named-mixin))
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
   "Intended to be mixed into classes representing parented model objects."))

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
   "Intended to be mixed into model classes with conditional instantiation."))

(defmethod instantiate? ((spec conditional-mixin) (parent t))
  (log:debug "~@<Checking whether to instantiate ~A with parent ~A~@:>"
             spec parent)
  (let+ (((&flet value (name)
            (let ((key (make-keyword (string-upcase name))))
              (or (value parent key nil)
                  (value spec key nil)))))
         ((&labels matches? (regex value)
            (etypecase value
              (null   nil)
              (cons   (some (curry #'matches? regex) value))
              (string (ppcre:scan-to-strings regex value))
              (t      (matches? regex (princ-to-string value)))))))
    (iter (for (expression . regex) in (conditions spec))
          (let* ((value    (value expression))
                 (matches? (matches? regex value)))
            (log:trace "~@<Checking ~S (=> ~S) against regex ~S => ~:[no ~
                        match~;~:*match ~S~]~@:>"
                       expression value regex matches?)
            (always matches?)))))
