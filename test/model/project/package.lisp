;;;; package.lisp --- Package definition for unit tests of the model.project module.
;;;;
;;;; Copyright (C) 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:build-generator.model.project.test
  (:use
   #:cl
   #:alexandria
   #:let-plus

   #:fiveam

   #:build-generator.model.project))

(cl:in-package #:build-generator.model.project.test)

(def-suite :build-generator.model.project
  :in :build-generator
  :description
  "Unit tests for the model.project module.")

(def-suite :build-generator.model.project.concrete-syntax
  :in :build-generator.model.project)
