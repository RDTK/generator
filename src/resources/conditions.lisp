;;;; conditions.lisp --- Conditions signaled by the resources module.
;;;;
;;;; Copyright (C) 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.project.resources)

(define-condition entry-does-not-exist-error (error)
  ((%name  :initarg :name
           :type    pathname
           :reader  name)
   (%group :initarg :group
           :reader  group))
  (:default-initargs
   :name  (missing-required-initarg 'entry-does-not-exist-error :name)
   :group (missing-required-initarg 'entry-does-not-exist-error :group))
  (:report
   (lambda (condition stream)
     (format stream "~@<An entry for ~S does not exist in ~A.~@:>"
             (name condition) (group condition)))))

(define-condition group-does-not-exist-error (error)
  ((%name      :initarg :name
               :type    symbol
               :reader  name)
   (%resources :initarg :resources
               :reader  resources))
  (:default-initargs
   :name      (missing-required-initarg 'group-does-not-exist-error :name)
   :resources (missing-required-initarg 'group-does-not-exist-error :resources))
  (:report
   (lambda (condition stream)
     (format stream "~@<A group named ~A does not exist in ~A.~@:>"
             (name condition) (resources condition)))))
