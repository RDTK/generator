;;;; util.lisp --- Utilities used in the model.project module.
;;;;
;;;; Copyright (C) 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.model.project)

(defun check-keys (object &optional expected (exhaustive? t))
  (let+ ((seen     '())
         (expected (mapcar #'ensure-list expected))
         (extra    '())
         ((&flet invalid-keys (reason keys)
            (error "~@<~A key~P: ~{~A~^, ~}.~@:>"
                   reason (length keys) keys))))
    (loop :for (key . value) :in object :do
       (cond
         ((member key seen :test #'eq)
          (error "~@<Duplicate key: ~A.~@:>" key))
         ((member key expected :test #'eq :key #'car)
          (removef expected key :test #'eq :key #'car))
         (t
          (push key extra)))
       (push key seen))
    (when (and exhaustive? extra)
      (invalid-keys "Unexpected" extra))
    (when-let ((missing (remove nil expected :key #'cdr)))
      (invalid-keys "Missing required" (mapcar #'car missing))))
  object)

(defun+ parse-dependency-spec ((nature name &optional version))
  (list* (make-keyword (string-upcase nature))
         name
         (etypecase version
           (list   version)
           (string (list (parse-version version))))))
