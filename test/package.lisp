;;;; package.lisp --- Package for tests of the jenkins.project system.
;;;;
;;;; Copyright (C) 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:jenkins.project.test
  (:use
   #:cl

   #:fiveam)

  (:export
   #:run-tests))

(cl:in-package #:jenkins.project.test)

(def-suite :jenkins.project)

(defun run-tests ()
  (run! :jenkins.project))
