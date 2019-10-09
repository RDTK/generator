;;;; progress.lisp --- Progress reports for operations.
;;;;
;;;; Copyright (C) 2014-2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:build-generator.model.project)

(defmethod build-generator.deployment:deploy :before ((thing job))
  (progress :deploy/job nil "~A" thing))
