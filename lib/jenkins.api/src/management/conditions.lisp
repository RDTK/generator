;;;; conditions.lisp --- Conditions used by the management module.
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.management)

(define-condition management-error (condition)
  ()
  (:documentation
   "TODO(jmoringe): document"))

(define-condition slave-condition (condition)
  ((slave :initarg  :slave
          :type     string
          :reader   slave-condition-slave
          :documentation
          ""))
  (:default-initargs
   :slave (missing-required-initarg 'slave-condition :slave))
  (:documentation
   "TODO(jmoringe): document"))

(define-condition install-error (management-error
                                 slave-condition
                                 chainable-condition)
  ((component :initarg  :component
              :reader   install-error-component
              :documentation
              ""))
  (:default-initargs
   :component (missing-required-initarg 'install-error :component))
  (:report
   (lambda (condition stream)
     (format stream "~@<Error installing ~A on slave ~S~/more-conditions::maybe-print-cause/~@:>"
             (install-error-component condition)
             (slave-condition-slave   condition)
             condition)))
  (:documentation
   "TODO(jmoringe): document"))
