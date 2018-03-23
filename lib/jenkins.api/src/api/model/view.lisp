;;;; view.lisp --- View model class.
;;;;
;;;; Copyright (C) 2012-2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.api)

(defparameter *default-columns*
  '("hudson.views.StatusColumn"
    "hudson.views.WeatherColumn"
    "hudson.views.BuildButtonColumn"
    "hudson.views.JobColumn"
    "hudson.views.LastSuccessColumn"
    "hudson.views.LastFailureColumn"
    "hudson.views.LastDurationColumn"))

(deftype string/column ()
  'string)

(defmethod xloc:xml-> ((value stp:element)
                       (type  (eql 'string/column))
                       &key inner-types)
  (declare (ignore inner-types))
  (stp:local-name value))

(defmethod xloc:->xml ((value string)
                       (dest  stp:element)
                       (type  (eql 'string/column))
                       &key inner-types)
  (declare (ignore inner-types))
  (setf (stp:local-name dest) value))

(define-model-class view ()
    ((id          :type      string
                  :xpath     "name/text()"
                  :optional? nil)
     (description :type      (or null string)
                  :xpath     "description/text()"
                  :initform  nil)
     (properties  :type      (singleton-element "text()")
                  :xpath     ("properties[@class=\"hudson.model.View$PropertyList\"]/property"
                              :if-multiple-matches :all)
                  :initform  '()
                  :optional? nil)
     (jobs        :type      (singleton-element "text()")
                  :xpath     ("jobNames[comparator[@class=\"hudson.util.CaseInsensitiveComparator\"]]/string"
                              :if-multiple-matches :all)
                  :initform  '()
                  :optional? nil)
     (job-filters :type      (singleton-element "text()")
                  :xpath     ("jobFilters/filter"
                              :if-multiple-matches :all)
                  :initform  '()
                  :optional? nil)
     (columns     :type      string/column
                  :xpath     ("columns/*"
                              :if-multiple-matches :all)
                  :initform  *default-columns*
                  :optional? nil))
  (:default-initargs
   :data (stp:make-document (stp:make-element "hudson.model.ListView")))
  (:get-func (lambda (id)      (view-config id)))
  (:put-func (lambda (id data) (setf (view-config id) data))))

;;; HACK: Jenkins requires a sorted list
;;; (w.r.t. hudson.util.CaseInsensitiveComparator).
(defmethod (setf jobs) :around ((new-value list) (view view))
  (call-next-method (sort (copy-list new-value) #'string-lessp) view))
