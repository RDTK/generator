;;;; package.lisp --- Package definition for unit tests of the management module.
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:jenkins.management.test
  (:use
   #:cl
   #:let-plus
   #:lift

   #:jenkins.management)

  (:documentation
   "This package contains unit tests for the management module"))

(cl:in-package #:jenkins.management.test)

(deftestsuite management-root ()
  ()
  (:documentation
   "Root unit test suite for the management module."))
