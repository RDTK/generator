;;;; ant.lisp --- Dummy analysis for ant projects.
;;;;
;;;; Copyright (C) 2015, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:build-generator.analysis)

(defmethod analyze ((source pathname) (kind (eql :ant)) &key)
  '())
