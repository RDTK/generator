;;;; build.lisp --- Build model class.
;;;;
;;;; Copyright (C) 2012-2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.api)

;;; `build' class

(define-model-class build ()
  ((building?  :type  boolean
               :xpath "building/text()")
   (slave-name :type  string
               :xpath "builtOn/text()")
   (result     :type  keyword
               :xpath "result/text()"))
  (:get-func (lambda (id) (build-config id))))

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
