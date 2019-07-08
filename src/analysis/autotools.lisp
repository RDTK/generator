;;;; autotools.lisp --- Dummy analysis for autotools projects.
;;;;
;;;; Copyright (C) 2015, 2017, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:build-generator.analysis)

(defmethod analyze ((source pathname) (kind (eql :autotools)) &key)
  `(:natures (,kind) :programming-languages ("C")))
