;;;; classes.lisp --- Classes used by the api module.
;;;;
;;;; Copyright (C) 2012-2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.api)

;;; `node' class

(define-model-class node ()
  ((name        :type      string)
   (description :type      string)
   (host        :type      string
                :xpath     "launcher/host/text()")
   (mode        :type      keyword)
   (label       :type      (list/space string)
                :xpath     "label/text()")
   (environment :type      tree-map/plist
                :xpath     "/slave/nodeProperties/hudson.slaves.EnvironmentVariablesNodeProperty/envVars/tree-map"))
  (:get-func (lambda (id)      (node-config id)))
  (:put-func (lambda (id data) (setf (node-config id) data)))
  (:name-slot nil))

(defmethod print-object ((object node) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A ~:[off~;on~]line"
            (name object) (online? object))))

;;; `build' class

(define-model-class build ()
    ((building?  :type  boolean
                 :xpath "building/text()")
     (slave-name :type  string
                 :xpath "builtOn/text()")
     (result     :type  keyword
                 :xpath "result/text()"))
  (:get-func (lambda (id) (build-config id)))
  (:name-slot nil))

(defmethod job ((build build) &key &allow-other-keys)
  (job (first (split-sequence #\/ (id build)))))

(defmethod slave ((build build) &key &allow-other-keys)
  (node (slave-name build)))

(defmethod failed? ((build build))
  (eq (result build) :failure))

(defmethod print-object ((object build) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A ~A"
            (id object) (if (building? object)
                            :in-progress
                            (result object)))))

;;; `item' class (Queue items)

(define-model-class item ()
    ((job-name :type  string
               :xpath "task/name/text()"))
  (:get-func (lambda (id) (item-config id))))

(defmethod job ((item item) &key &allow-other-keys)
  (job (job-name item)))
