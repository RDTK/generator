;;;; package.lisp --- Package for tests of the model.variables module.
;;;;
;;;; Copyright (C) 2018, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:jenkins.model.variables.test
  (:use
   #:cl
   #:alexandria
   #:let-plus

   #:fiveam

   #:jenkins.model.variables)

  (:import-from #:jenkins.model.variables
   #:merge-alists))

(cl:in-package #:jenkins.model.variables.test)

(def-suite :jenkins.project.model.variables
  :in :jenkins.project)
