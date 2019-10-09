;;;; protocol.lisp --- Protocol provided by the deployment module.
;;;;
;;;; Copyright (C) 2012-2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:build-generator.deployment)

;;; Deployment protocol

(defgeneric deploy (thing)
  (:documentation
   "Deploy THING.

    Signal `deployment-condition's such as `deployment-error' when
    conditions such as errors are encountered."))

(defgeneric deploy-dependencies (thing)
  (:documentation
   "Deploy dependencies of THING.

    Signal `deployment-condition's such as `deployment-error' when
    conditions such as errors are encountered."))

;;; Default behavior

(defmethod deploy :around ((thing t))
  (with-condition-translation (((error deployment-error) :thing thing))
    (with-simple-restart (continue "~@<Skip deployment of ~A.~@:>" thing)
      (call-next-method))))

(defmethod deploy-dependencies :around ((thing t))
  (with-condition-translation (((error deployment-error) :thing thing))
    (with-simple-restart (continue "~@<Skip deploying dependencies of ~
                                    ~A.~@:>"
                                   thing)
      (call-next-method))))
