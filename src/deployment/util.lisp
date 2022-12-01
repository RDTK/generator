;;;; util.lisp --- Utilities provided by the deployment module.
;;;;
;;;; Copyright (C) 2018-2022 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:build-generator.deployment)

(defun job-full-name (thing &key (separator "@"))
  (let* ((version (model:parent thing))
         (project (model:parent (model:specification version))))
    (format nil "~A~A~A"
            (model:name project) separator (model:name version))))

(defun print-heading (stream title &key (width 80))
  (format stream "##~V,,,'#<~>##~@
                  # ~:*~V<~A~;~> #~@
                  ~2:*##~V,,,'#<~>##~2%"
          width title))
