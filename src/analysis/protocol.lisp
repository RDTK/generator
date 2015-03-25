;;;; protocol.lisp --- Protocol for the analysis module.
;;;;
;;;; Copyright (C) 2012, 2013, 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.analysis)

;;; Analysis protocol

(defgeneric analyze (source kind &key &allow-other-keys)
  (:documentation
   "TODO"))

(defmethod analyze :around ((source t) (kind t) &key &allow-other-keys)
  (with-condition-translation (((error analysis-error)
                                :specification source))
    (let ((result (multiple-value-list (call-next-method))))
      (log:debug "~@<Analysis result for ~A ~A: ~S~@:>"
                 source kind result)
      (apply #'values result))))
