;;;; mixins.lisp --- Generic mixin classes used by project, templates, etc.
;;;;
;;;; Copyright (C) 2012-2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.model.variables)

;;; `direct-variables-mixin'

(defclass direct-variables-mixin ()
  ((variables :initarg  :variables
              :type     list ; alist
              :accessor %direct-variables
              :reader   direct-variables
              :initform '()
              :documentation
              "Stores direct variables definitions as an alist with
               elements of the form

                 (NAME . EXPRESSION)

               where NAME is a keyword naming the variable and
               EXPRESSION a `variable-expression', the unevaluated
               value of the variable."))
  (:documentation
   "Adds a list of direct variable definition cells."))

(defmethod shared-initialize :around ((instance   direct-variables-mixin)
                                      (slot-names t)
                                      &key)
  (call-next-method)
  (loop :for cell :in (%direct-variables instance) :do
     (unless (gethash cell *variable-locations*)
       (setf (gethash cell *variable-locations*) instance))))

(defmethod variables append ((thing direct-variables-mixin))
  (copy-list (direct-variables thing)))

(defmethod direct-lookup ((thing direct-variables-mixin) (name t))
  (if-let ((cell (find name (direct-variables thing)
                       :test #'eq
                       :key  #'car)))
    (values cell '() t)
    (values nil  '() nil)))

(defmethod lookup ((thing direct-variables-mixin) (name t)
                   &key if-undefined)
  (declare (ignore if-undefined))
  (direct-lookup thing name))

(defmethod (setf lookup) ((new-value t)
                          (thing     direct-variables-mixin)
                          (name      t)
                          &key if-undefined)
  (declare (ignore if-undefined))
  (removef (%direct-variables thing) name :key #'car)
  (let ((cell (value-cons name new-value)))
    (push cell (%direct-variables thing))
    (setf (gethash cell *variable-locations*) thing)
    new-value))

;;; `builtin-entries-mixin'

(defclass builtin-entries-mixin ()
  ())

(defmethod shared-initialize :after ((instance   builtin-entries-mixin)
                                     (slot-names t)
                                     &key
                                     (variables nil variables-supplied?))
  (declare (ignore variables))
  (when variables-supplied?
    (loop :for (name . value) :in (builtin-entries instance)
          :do (if-let ((cell (assoc name (%direct-variables instance) :test #'eq)))
                (setf (cdr cell) value)
                (push (cons name value) (%direct-variables instance))))))
