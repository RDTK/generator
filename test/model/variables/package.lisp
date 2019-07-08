;;;; package.lisp --- Package for tests of the model.variables module.
;;;;
;;;; Copyright (C) 2018, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:build-generator.model.variables.test
  (:use
   #:cl
   #:alexandria
   #:let-plus

   #:fiveam

   #:build-generator.model.variables)

  (:import-from #:build-generator.model.variables
   #:merge-alists))

(cl:in-package #:build-generator.model.variables.test)

(def-suite :build-generator.model.variables
  :in :build-generator)
