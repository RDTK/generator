;;;; package.lisp --- Package definition for cl-jenkins system.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:jenkins.management
  (:use
   #:cl
   #:alexandria
   #:split-sequence
   #:let-plus
   #:iterate
   #:more-conditions

   #:inferior-shell

   #:jenkins.api)

  ;; Conditions
  (:export
   #:slave-condition
   #:slave-condition-slave

   #:management-error

   #:install-error
   #:install-error-component)

  ;; Variables
  (:export
   :*sbcl-version*)

  (:export
   #:install-sbcl

   #:install-quicklisp
   #:setup-slaves)

  ;; High-level API
  (:export
   :setup-slave)

  (:documentation
   "TODO"))
