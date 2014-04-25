;;;; scm-null.lisp --- Analysis for projects without repository.
;;;;
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.analysis)

(defmethod analyze ((source (eql nil)) (kind t)
                    &key
                    (branches (missing-required-argument :branches)))
  (mapcar (lambda (branch) (cons branch '())) branches))
