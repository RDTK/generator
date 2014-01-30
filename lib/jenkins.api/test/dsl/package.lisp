;;;; package.lisp --- Package definition for unit tests of the dsl module.
;;;;
;;;; Copyright (C) 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:jenkins.dsl.test
  (:use
   #:cl
   #:lift

   #:jenkins.dsl)

  (:export
   #:dsl-root)

  (:documentation
   "This package contains unit tests for the jenkins.dsl system"))

(cl:in-package #:jenkins.dsl.test)

(deftestsuite dsl-root ()
  ()
  (:documentation
   "Root unit test suite for the jenkins.dsl system."))
