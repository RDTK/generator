;;;; util.lisp --- Utilities used in different kinds of reports.
;;;;
;;;; Copyright (C) 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.report)

(defun safe-name (name)
  (substitute #\_ #\/ name))
