;;;; parser.lisp --- TODO.
;;;;
;;;; Copyright (C) 2016, 2017, 2018, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.language-server)

(defun parse-with-parser (parser source)
  (let ((project::*locations* (make-hash-table :test #'eq))
        (project::*locations-lock* (bt:make-lock "parser")))
    (values (funcall parser source :generator-version "0.20.0")
            project::*locations*)))

(defun parse-template (source file)
  (let ((project::*templates* (make-hash-table :test #'equal))
        (project::*templates-lock* (bt:make-lock "template parser")))
    (multiple-value-call #'values
      (parse-with-parser (rcurry #'project::load-one-template/json :pathname file :content source) ; load-template/json source
                         (make-string-input-stream source)) ; TODO should accept string as well
      project::*templates*)))

(let ((filename "~/code/citec/citk/recipes/templates/_common/job-defaults.template"))
  (parse-template (alexandria:read-file-into-string filename) filename))
