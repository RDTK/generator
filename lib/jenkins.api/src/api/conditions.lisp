;;;; conditions.lisp --- Conditions used by the api module.
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.api)

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
interface cannot be found during deserializing of a model object from
XML."))
