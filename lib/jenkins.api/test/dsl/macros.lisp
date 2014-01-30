;;;; macros.lisp --- Unit tests for macros provided by the dsl module.
;;;;
;;;; Copyright (C) 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.dsl.test)

(job (:matrix "foo")
  (repositories
   (git (:url                  "foo"
	 :branches             '("master")
	 :checkout-submodules? t))))
