;;;; util.lisp --- Utilities for the recipe concrete syntax.
;;;;
;;;; Copyright (C) 2017, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.model.project)

;;; Structure utilities

(defun check-keys (object &optional expected (exhaustive? t))
  (let+ ((expected (mapcar #'ensure-list expected))
         (seen     '())
         (extra    '())
         ((&flet invalid-keys (reason keys &optional cells)
            (object-error
             (if (length= 1 cells)
                 (list (list (first cells) "defined here" :error))
                 (loop :for cell :in cells
                       :for i :downfrom (length cells)
                       :collect (list cell (format nil "~:R definition" i) :error)))
             "~@<~A key~P: ~{~A~^, ~}.~@:>"
             reason (length keys) keys))))
    (map nil (lambda+ ((&whole cell key . value))
               (cond
                 ((member key seen :test #'eq :key #'car)
                  (invalid-keys "duplicate" (list key)
                                (list* cell (remove key seen
                                                    :test-not #'eq
                                                    :key      #'car))))
                 ((when-let ((info (find key expected :test #'eq :key #'first)))
                    (when-let ((type  (third info)))
                      (unless (typep value type)
                        (object-error
                         (list (list value "defined here" :error))
                         "~@<Value of the ~A attribute must be of type ~A.~@:>"
                         key type)))
                    (removef expected key :test #'eq :key #'first)
                    t))
                 (t
                  (push cell extra)))
               (push cell seen))
         object)
    (when (and exhaustive? extra)
      (invalid-keys "Unexpected" (map 'list #'first extra)
                    extra))
    (when-let ((missing (remove nil expected :key #'second)))
      (invalid-keys "Missing required" (map 'list #'first missing)
                    (list object))))
  object)

(defun check-generator-version (spec generator-version context)
  (when-let ((required-version (assoc-value spec :minimum-generator-version)))
    (unless (version-matches (parse-version required-version)
                             (parse-version generator-version))
      (object-error
       (list (list required-version "minimum version declaration" :info))
       "~@<The ~A requires generator version ~S, but this ~
        generator is version ~S.~@:>"
       context required-version generator-version))))

(defun process-variables (alist)
  (let ((entries (make-hash-table :test #'eq)))
    (map nil (lambda+ ((&whole cell key . &ign))
               (when (starts-with-subseq "__" (string key))
                 (object-error
                  (list (list cell "variable definition" :error))
                  "~@<Variable name ~A starts with \"__\". These ~
                   variable names are reserved for internal use.~@:>"
                  key))
               (push cell (gethash key entries)))
         alist)
    (loop :for key :being :the :hash-key :of entries
          :using (:hash-value cells)
          :unless (length= 1 cells)
          :do (object-error
               (loop :for cell :in cells
                     :for i :downfrom (length cells)
                     :collect (list cell (format nil "~:R definition" i)
                                    (if (= i 1) :note :error)))
               "~@<Multiple definitions of variable ~A.~@:>"
               key)
          :collect (value-cons key (cdr (first cells))))))
