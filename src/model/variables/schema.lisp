;;;; schema.lisp --- Meta-data and checks for defined variables..
;;;;
;;;; Copyright (C) 2014-2019, 2022 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:build-generator.model.variables)

;;; Variable schema machinery

(deftype inheritance-mode ()
  '(member nil :outermost-only t))

(deftype aggregation-mode ()
  '(member nil :append :merge :histogram))

(defclass variable-info (print-items:print-items-mixin)
  ((name          :initarg  :name
                  :type     keyword
                  :reader   variable-info-name)
   (type          :initarg  :type
                  :reader   variable-info-type)
   (inheritance   :initarg  :inheritance
                  :type     inheritance-mode
                  :reader   inheritance
                  :initform t
                  :documentation
                  "Stores the inheritance mode for the variable.

                   The mode is used when the value of a variable is
                   computed in a child object that does have a
                   definition of the variable but has a parent that
                   has.")
   (aggregation   :initarg  :aggregation
                  :type     aggregation-mode
                  :reader   aggregation
                  :initform nil
                  :documentation
                  "Stores the aggregation mode for the variable.

                   The mode is used when the value of a variable is
                   computed in a parent object that does not have a
                   definition of the variable based on the values of
                   the variable in its child objects.")
   (documentation :initarg  :documentation
                  :type     (or null string)
                  :reader   variable-info-documentation
                  :initform nil))
  (:default-initargs
   :name (missing-required-initarg 'variable-info :name)
   :type (missing-required-initarg 'variable-info :type)))

(defun make-variable-info (name type &key inheritance
                                          aggregation
                                          documentation)
  (make-instance 'variable-info
                 :name          name
                 :type          type
                 :inheritance   inheritance
                 :aggregation   aggregation
                 :documentation documentation))

(defmethod print-items:print-items append ((object variable-info))
  (let+ (((&structure-r/o variable-info- name type) object))
    `((:name                  "~A"   ,name)
      ((:type (:after :name)) ": ~A" ,type))))

(defvar *variables* (make-hash-table :test #'eq))

(defvar *variable-uses* (make-hash-table :test #'eq))

(defun all-variables ()
  (hash-table-values *variables*))

(defun find-variable (name &key if-does-not-exist)
  (or (gethash name *variables*)
      (error-behavior-restart-case
          (if-does-not-exist
           (simple-error
            :format-control   "~@<~S does not designate a variable.~@:>"
            :format-arguments (list name))))))

(defun (setf find-variable) (info name &key if-does-not-exist)
  (declare (ignore if-does-not-exist))
  (setf (gethash name *variables*) info))

(defun note-variable (name type &key assume-used?
                                     (inheritance  t)
                                     (aggregation  nil)
                                     documentation)
  (let ((variable (if-let ((existing (find-variable name)))
                    (reinitialize-instance existing
                                           :name          name
                                           :type          type
                                           :inheritance   inheritance
                                           :aggregation   aggregation
                                           :documentation documentation)
                    (setf (find-variable name)
                          (make-variable-info name type
                                              :inheritance   inheritance
                                              :aggregation   aggregation
                                              :documentation documentation)))))
    (when assume-used?
      (note-variable-use variable))
    variable))

(defun note-variable-use (variable)
  (incf (gethash variable *variable-uses* 0)))

;;; Macros

(defmacro define-variable (name type
                           &key (inheritance nil inheritance-supplied?)
                                (aggregation nil aggregation-supplied?)
                                documentation)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (note-variable ',name ',type
                    ,@(when inheritance-supplied?
                        `(:inheritance ,inheritance))
                    ,@(when aggregation-supplied?
                        `(:aggregation ,aggregation))
                    :documentation ,documentation)))

;;; Compile-time checks

(defun check-variable-liveness ()
  (maphash (lambda (name variable)
             (unless (gethash variable *variable-uses*)
               (warn 'unused-variable-warning :name name)))
           *variables*))

(defun check-variable-access (name &key (if-undefined #'warn))
  (if-let ((variable (find-variable name)))
    (progn
      (note-variable-use variable)
      variable)
    (error-behavior-restart-case
        (if-undefined (undefined-variable-error :name name)
                      :warning-condition undefined-variable-warning)
      (use-value (value)
        value))))

(labels ((check-variable-name-form (name)
           (when (constantp name)
             (let ((name (eval name)))
               (when (keywordp name)
                 (check-variable-access name)))))
         (load-time-note-use (name function form)
           `(locally (declare (notinline ,function))
              (load-time-value (note-variable-use (find-variable ',name)))
              ,form)))

  (define-compiler-macro lookup (&whole form thing name &key if-undefined)
    (declare (ignore thing if-undefined))
    (if (check-variable-name-form name)
        (load-time-note-use name 'lookup form)
        form))

  (define-compiler-macro value (&whole form thing name &optional default)
    (declare (ignore thing default))
    (if (check-variable-name-form name)
        (load-time-note-use name 'value form)
        form))

  (define-compiler-macro value/cast (&whole form thing name &optional default)
    (declare (ignore thing default))
    (if-let ((variable (check-variable-name-form name)))
      (let ((type (variable-info-type variable)))
        (load-time-note-use
         name 'value/cast
         `(multiple-value-bind (value defaulted?) ,form
            (values (the ,type value) defaulted?))))
      form)))
