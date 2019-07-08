;;;; package.lisp --- Package for tests of the build-generator system.
;;;;
;;;; Copyright (C) 2018, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:build-generator.test
  (:use
   #:cl

   #:fiveam)

  (:export
   #:run-tests))

(cl:in-package #:build-generator.test)

(def-suite :build-generator)

(defun run-tests ()
  (run! :build-generator))
