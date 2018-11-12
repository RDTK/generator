;;;; progress.lisp --- Progress reports for operations.
;;;;
;;;; Copyright (C) 2014, 2015, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.model.project)

(defmethod deploy :around ((thing version))
  (with-sequence-progress (:deploy/job (jobs thing))
    (call-next-method)))

(defmethod deploy :before ((thing job))
  (progress :deploy/job nil "~A" thing))
