;;;; conditions.lisp --- Conditions used by the api module.
;;;;
;;;; Copyright (C) 2012-2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.api)

;;; Request-related errors

(define-condition request-failed-error (error)
  ((%endpoint     :initarg :endpoint
                  :reader  endpoint)
   (%relative-url :initarg :relative-url
                  :reader  relative-url)
   (%code         :initarg :code
                  :reader  code)
   (%body         :initarg :body
                  :reader  body))
  (:report
   (lambda (condition stream)
     (format stream "~@<Request to ~A failed (code ~D):~@:_~
                     ~@:_~
                     ~2@T~@<~@:;~A~@:>~@:>"
             (absolute-url condition) (code condition) (body condition))))
  (:documentation
   "Signaled when an HTTP request fails."))

(defmethod absolute-url ((condition request-failed-error))
  (puri:merge-uris (relative-url condition)
                   (base-url (endpoint condition))))

(define-condition object-not-found-error (request-failed-error)
  ()
  (:report
   (lambda (condition stream)
     (format stream "~@<Object not found at ~A (code ~D):~@:_~
                     ~@:_~
                     ~2@T~@<~@:;~A~@:>~@:>"
             (absolute-url condition) (code condition) (body condition))))
  (:documentation
   "Signaled when an object cannot be found on the server."))

;;; Higher-level communication conditions

(define-condition communication-condition (condition)
  ((%endpoint :initarg :endpoint
              :reader  endpoint))
  (:default-initargs
   :endpoint (missing-required-initarg 'communication-condition :endpoint))
  (:documentation
   "Supertype for high-level communication conditions."))

(define-condition failed-to-obtain-csrf-token-error (error
                                                     communication-condition
                                                     more-conditions:chainable-condition)
  ()
  (:report
   (lambda (condition stream)
     (format stream "~@<Could not obtain CSRF token from Jenkins ~
                     instance at ~A.~
                     ~/more-conditions:maybe-print-cause/~@:>"
             (base-url (endpoint condition)) condition)))
  (:documentation
   "Signaled when a CSRF token cannot be obtained."))

(define-condition jenkins-connect-error (error
                                         communication-condition
                                         more-conditions:chainable-condition)
  ()
  (:report
   (lambda (condition stream)
     (format stream "~@<Could not connect to Jenkins instance at ~A.~
                     ~/more-conditions:maybe-print-cause/~@:>"
             (base-url (endpoint condition)) condition)))
  (:documentation
   "Signaled when an initial connection to a Jenkins instance fails."))

;;; Serialization-related conditions

(define-condition unmapped-class (condition)
  ((interface :initarg :interface
              :type    symbol
              :reader  unmapped-class-interface
              :documentation
              "Name of the interface for which the named
               implementation could not be found.")
   (name      :initarg :name
              :type    string
              :reader  unmapped-class-name
              :documentation
              "Name of the implementation which could not be found."))
  (:default-initargs
   :interface (missing-required-initarg 'unmapped-class :interface)
   :name      (missing-required-initarg 'unmapped-class :name))
  (:report (lambda (condition stream)
             (format stream "~@<No mapping for implementation ~S of ~
                                interface ~S.~@:>"
                     (unmapped-class-name condition)
                     (unmapped-class-interface condition))))
  (:documentation
   "This condition is signaled when a named implementation of an
    interface cannot be found during deserializing of a model object
    from XML."))
