;;;; ant.lisp --- Dummy analysis for ant projects.
;;;;
;;;; Copyright (C) 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.analysis)

(defmethod analyze ((source pathname) (kind (eql :ant)) &key)
  '())
