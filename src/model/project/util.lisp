;;;; util.lisp --- Utilities used in the model.project module.
;;;;
;;;; Copyright (C) 2017, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.model.project)

(defun+ parse-dependency-spec ((nature name &optional version))
  (list* (make-keyword (string-upcase nature))
         name
         (etypecase version
           (list   version)
           (string (list (parse-version version))))))
