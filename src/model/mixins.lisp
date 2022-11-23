;;;; mixins.lisp --- Generic mixin classes used by project, templates, etc.
;;;;
;;;; Copyright (C) 2012-2019, 2022 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:build-generator.model)

;;; `named-mixin'

(declaim (inline name-character?))
(defun name-character? (character)
  (or (alphanumericp character)
      (find character "._-+/!@")))

(defun name? (thing)
  (and (stringp thing)
       (not (emptyp thing))
       (every #'name-character? thing)))

(deftype name ()
  `(and string (satisfies name?)))

(defclass named-mixin (print-items:print-items-mixin)
  ((name :initarg  :name
         :type     name
         :reader   name
         :documentation
         "The name of the model object.

          Should be unique within the object's associated
          namespace."))
  (:documentation
   "Intended to be mixed into classes representing named model objects."))

(defmethod print-items:print-items append ((object named-mixin))
  `((:name "~{~A~^:~}" ,(reverse (ancestor-names object)))))

;;; `named+builtin-entries-mixin'

(defclass named+builtin-entries-mixin (named-mixin
                                       var:builtin-entries-mixin)
  ())

(defmethod var:builtin-entries append ((thing named+builtin-entries-mixin))
  `((,(name-variable thing) . ,(name thing))))

;;; `parented-mixin'

(defclass parented-mixin ()
  ((parent :initarg  :parent
           :reader   parent
           :initform nil
           :documentation
           ""))
  (:documentation
   "Intended to be mixed into classes representing parented model objects."))

(defmethod var:variables append ((thing parented-mixin))
  (when-let ((parent (parent thing)))
    (var:variables parent)))

(defmethod var:lookup ((thing parented-mixin) (name t) &key if-undefined)
  (declare (ignore if-undefined))
  (let ((next-method? (next-method-p))
        (parent       (parent thing)))
    (cond ((and next-method? parent)
           (multiple-value-call #'var:merge-lookup-values
             (call-next-method) (var:lookup parent name :if-undefined nil)))
          (next-method?
           (call-next-method))
          (parent
           (var:lookup parent name :if-undefined nil))
          (t
           (values nil nil nil)))))

;;; `implementation-mixin'

(defclass implementation-mixin ()
  ((specification :initarg  :specification
                  :reader   specification
                  :accessor %specification
                  :initform nil
                  :documentation
                  "Stores the specification according to which the
                   implementation object has been created."))
  (:documentation
   "Instances of this class may have associated specification objects
    of which they are implementations."))

;;; `specification-mixin'

(defclass specification-mixin ()
  ((implementations :initarg  :implementation
                    :type     list
                    :accessor implementations
                    :initform '()
                    :documentation
                    "Stores all implementations that have been created
                     according to the specification object."))
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
            (let+ (((&values value defaulted?) (var:value parent name nil)))
              (if defaulted?
                  (var:value spec name nil)
                  value))))
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
