;;;; util.lisp --- Utilities used in the model.aspects module.
;;;;
;;;; Copyright (C) 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.model.aspects)

(defun format-constraints (stream constraints &optional colon? at?)
  (declare (ignore colon? at?))
  (format stream "~:[~
                    <no constraints>~
                  ~;~
                    ~:*~{• ~{~6A ~A~^ ~A~}~^~@:_~}~
                  ~]"
          constraints))
