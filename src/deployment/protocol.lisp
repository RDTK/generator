;;;; protocol.lisp --- Protocol provided by the deployment module.
;;;;
;;;; Copyright (C) 2012-2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:build-generator.deployment)

;;; Deployment protocol

(defgeneric deploy (thing target)
  (:documentation
   "Deploy THING for TARGET.

    Signal `deployment-condition's such as `deployment-error' when
    conditions such as errors are encountered."))

(defgeneric deploy-dependencies (thing target)
  (:documentation
   "Deploy dependencies of THING for TARGET.

    Signal `deployment-condition's such as `deployment-error' when
    conditions such as errors are encountered."))

;; Default behavior

(defmethod deploy :around ((thing t) (target t))
  (with-condition-translation (((error deployment-error)
                                :thing thing :target target))
    (with-simple-restart (continue "~@<Skip deployment of ~A for ~A.~@:>"
                                   thing target)
      (call-next-method))))

(defmethod deploy-dependencies :around ((thing t) (target t))
  (with-condition-translation (((error deployment-error)
                                :thing thing :target target))
    (with-simple-restart (continue "~@<Skip deploying dependencies of ~
                                    ~A for ~A.~@:>"
                                   thing target)
      (call-next-method))))

;;; Service

(service-provider:define-service target
  (:documentation
   "Providers implement different kinds of deployments."))

(defun make-target (kind &rest initargs &key &allow-other-keys)
  (apply #'service-provider:make-provider 'target kind initargs))
