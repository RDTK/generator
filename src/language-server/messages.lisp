;;;; messages.lisp --- TODO.
;;;;
;;;; Copyright (C) 2016, 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.language-server)

(defun parse-position (thing)
  (values (the non-negative-integer (assoc-value thing :line))
          (the non-negative-integer (assoc-value thing :character))))

(defun parse-range (thing)
  (multiple-value-call #'values
    (parse-position (assoc-value thing :start))
    (parse-position (assoc-value thing :end))))

(defun parse-location (thing)
  (multiple-value-call #'values
    (values (assoc-value thing :uri))
    (parse-range (assoc-value thing :range))))

(defun parse-text-document (thing)
  (values (assoc-value thing :uri)
          (assoc-value thing :version)))

(defun parse-text-document-content-change (thing)
  (list (assoc-value thing :text)
        (when-let ((range (assoc-value thing :range)))
          (multiple-value-list (parse-range range)))
        (when-let ((range-length (assoc-value thing :range--length)))
          (parse-integer range-length))))
