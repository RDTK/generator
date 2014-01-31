;;;; progress.lisp --- Progress reports for operations.
;;;;
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.project)

(defmethod deploy :around ((thing project))
  (with-sequence-progress (:deploy/version (versions thing))
    (call-next-method)))

(defmethod deploy :around ((thing version))
  (progress :deploy/version nil "~A" thing)
  (with-sequence-progress (:deploy/job (jobs thing))
    (call-next-method)))

(defmethod deploy :before ((thing job))
  (progress :deploy/job nil "~A" thing))
