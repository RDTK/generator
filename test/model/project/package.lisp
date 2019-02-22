;;;; package.lisp --- Package definition for unit tests of the model.project module.
;;;;
;;;; Copyright (C) 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:jenkins.model.project.test
  (:use
   #:cl
   #:let-plus

   #:fiveam

   #:jenkins.model.project))

(cl:in-package #:jenkins.model.project.test)

(def-suite :jenkins.project.model.project
  :in :jenkins.project
  :description
  "Unit tests for the model.project module.")
