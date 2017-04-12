;;;; view.lisp --- View model class.
;;;;
;;;; Copyright (C) 2012-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.api)

(define-model-class view ()
    ((id          :type     string
                  :xpath    "name/text()"
                  :optional? nil)
     (description :type     (or null string)
                  :xpath    "description/text()"
                  :initform nil)
     (jobs        :type     (singleton-element "text()")
                  :xpath    ("jobNames[comparator[@class=\"hudson.util.CaseInsensitiveComparator\"]]/string"
                             :if-multiple-matches :all)))
  (:default-initargs
   :data (stp:make-document (stp:make-element "hudson.model.ListView")))
  (:get-func (lambda (id)      (view-config id)))
  (:put-func (lambda (id data) (setf (view-config id) data))))

;;; HACK: Jenkins requires a sorted list
;;; (w.r.t. hudson.util.CaseInsensitiveComparator).
(defmethod (setf jobs) :around ((new-value list) (view view))
  (call-next-method (sort (copy-list new-value) #'string-lessp) view))
