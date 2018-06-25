;;;; aggregation.lisp --- Aggregation of variable values.
;;;;
;;;; Copyright (C) 2012-2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.model.variables)

;;; Histogram strategy

(defmethod aggregate-values ((value    t)
                             (children t)
                             (name     t)
                             (strategy (eql :histogram)))
  (let ((counts (make-hash-table :test #'eq)))
    (map nil (lambda (child)
               (map nil (lambda (value)
                          (incf (gethash (make-keyword value) counts 0)))
                    (value child name '())))
         children)
    (hash-table-alist counts)))
