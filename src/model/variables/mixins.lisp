;;;; mixins.lisp --- Generic mixin classes used by project, templates, etc.
;;;;
;;;; Copyright (C) 2012, 2013, 2014, 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.model.variables)

;;; `direct-variables-mixin'

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
