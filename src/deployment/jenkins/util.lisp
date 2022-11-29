;;;; util.lisp --- Utilities used in the deployment.jenkins module.
;;;;
;;;; Copyright (C) 2019, 2022 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:build-generator.deployment.jenkins)

(defun jenkins-job-id (job)
  (substitute-if-not #\_ #'jenkins.api:job-name-character?
                     (var:value/cast job :build-job-name)))
