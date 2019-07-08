;;;; protocol.lisp --- Protocol provided by the steps module.
;;;;
;;;; Copyright (C) 2014-2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:build-generator.steps)

;;; Step protocol

(defgeneric execute (step context &key &allow-other-keys)
  (:documentation
   "Execute STEP in CONTEXT.

    Input data items are supplied via keyword parameters."))

;;; Step creation protocol

(defgeneric make-step (spec &rest args)
  (:documentation
   "Create and return a step according to SPEC and ARGS."))

;;; Default behavior

(defmethod make-step ((spec t) &rest args)
  (apply #'service-provider:make-provider 'step spec args))

;;; Step service

(service-provider:define-service step
  (:documentation
   "Providers of this service consume zero or more input items,
    perform some coherent operation and produce zero or more output
    items.

    The execution of such steps is usually organized in a plan to
    perform larger computations."))
