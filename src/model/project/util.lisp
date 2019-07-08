;;;; util.lisp --- Utilities used in the model.project module.
;;;;
;;;; Copyright (C) 2017, 2018, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:build-generator.model.project)

(defun parse-dependency-spec (spec)
  (flet ((result (nature target &optional version)
           (list* (make-keyword (string-upcase nature))
                  target
                  (etypecase version
                    (null   '())
                    (cons   version)
                    (string (list (parse-version version)))))))
    (optima:match spec
      ;; Legacy syntax: [ "NATURE", "TARGET", VERSION ]
      ((list* (and nature (type string))
              (and target (type string))
              (or (list (and version (type string))) '()))
       (result nature target version))

      ;; New syntax:
      ;; nature: NATURE
      ;; target: TARGET
      ;; version: VERSION
      ((and (assoc :nature (and nature (type string)))
            (assoc :target (and target (type string)))
            (or (assoc :version (and version (type string)))
                (and)))
       (check-keys spec '((:nature  t   string)
                          (:target  t   string)
                          (:version nil string)))
       (result nature target version))

      (otherwise
       (object-error
        (list (list spec "specified here" :error))
        "~@<Dependency specification must be a list of two or three ~
         elements or a dictionary with keys \"nature\", \"target\" and ~
         optionally \"version\".~@:>")))))
