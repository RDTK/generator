;;;; scm-null.lisp --- Analysis for projects without repository.
;;;;
;;;; Copyright (C) 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.analysis)

(defmethod analyze ((source (eql nil)) (kind t)
                    &key
                    (versions (missing-required-argument :versions)))
  (mapcar (lambda (version) (cons version '())) versions))
