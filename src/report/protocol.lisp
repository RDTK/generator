;;;; protocol.lisp --- Protocol provided by the report module.
;;;;
;;;; Copyright (C) 2015, 2016, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:build-generator.report)

(defgeneric report (object style target)
  (:documentation
   "Send report for OBJECT with STYLE to TARGET.

    TARGET is usually a stream."))
