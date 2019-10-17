;;;; util.lisp --- Utilities for the recipe concrete syntax.
;;;;
;;;; Copyright (C) 2017, 2018, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:build-generator.model.project)

;;; Structure utilities

(defun check-keys (object &optional expected (exhaustive? t))
  (let+ ((schema  (mapcar #'ensure-list expected))
         (missing (remove nil schema :key #'second))
         (seen    '())
         (extra   '())
         ((&flet closest-matches (invalid-keys)
            (reduce (rcurry #'nunion :test #'string-equal)
                    (map 'list #'string invalid-keys)
                    :key (rcurry #'util:closest-matches
                                 (map 'list (compose #'first #'ensure-list)
                                      expected)
                                 :key #'string))))
         ((&flet invalid-keys (reason keys &optional cells suggest?)
            (let ((candidates (when suggest? (closest-matches keys))))
              (object-error
               (if (length= 1 cells)
                   (list (list (first cells) "defined here" :error))
                   (loop :for cell :in cells
                         :for i :downfrom (length cells)
                         :collect (list cell (format nil "~:R definition" i) :error)))
               "~@<~A key~P: ~{~A~^, ~}.~@[ Closest match~[~;~:;es~]: ~
               ~{~A~^, ~}.~]~@:>"
               reason (length keys) keys
               (when candidates (length candidates)) candidates)))))
    (map nil (lambda+ ((&whole cell key . value))
               (cond
                 ((member key seen :test #'eq :key #'car)
                  (invalid-keys "duplicate" (list key)
                                (list* cell (remove key seen
                                                    :test-not #'eq
                                                    :key      #'car))))
                 ((when-let ((info (find key schema :test #'eq :key #'first)))
                    (let+ (((&ign &optional required? type &key conflicts) info))
                      (when (and type (not (typep value type)))
                        (object-error
                         (list (list value "defined here" :error))
                         "~@<Value of the ~A attribute must be of type
                          ~A.~@:>"
                         key type))
                      (when conflicts
                        (when-let ((other (find conflicts seen
                                                :test #'eq :key #'car)))
                          (object-error
                           (list (list cell  "defined here" :error)
                                 (list other "defined here" :error))
                           "~@<~A and ~A are mutually exclusive.~@:>"
                           key conflicts)))
                      (when (and required? conflicts)
                        (removef missing conflicts :test #'eq :key #'first)))
                    (removef missing key :test #'eq :key #'first)
                    t))
                 (t
                  (push cell extra)))
               (push cell seen))
         object)
    (when (and exhaustive? extra)
      (invalid-keys "Unexpected" (map 'list #'first extra)
                    extra t))
    (when missing
      (invalid-keys "Missing required" (map 'list #'first missing)
                    (list object))))
  object)

(let ((spec '((:name    t string :conflicts :pattern)
              (:pattern t string :conflicts :name))))
  (assert (nth-value
           1 (ignore-errors
              (check-keys '((:name . "foo") (:pattern . "bar")) spec))))
  (assert (equal '((:name . "foo")) (check-keys '((:name . "foo")) spec)))
  (assert (equal '((:pattern . "bar")) (check-keys '((:pattern . "bar")) spec))))

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
          :collect (let ((new-cell (handler-bind ((esrap:esrap-parse-error
                                                    (lambda (condition)
                                                      (let* ((location (location-of (cdr (first cells))))
                                                             (start    (text.source-location:index
                                                                        (text.source-location:start location))))
                                                        (log:error location (esrap:esrap-error-position condition)
                                                                   (+ start
                                                                      (esrap:esrap-error-position condition)))
                                                        (error 'simple-object-error
                                                               :annotations (list (text.source-location:make-annotation
                                                                                   (text.source-location:make-location
                                                                                    (text.source-location:source location)
                                                                                    start (1+ start))
                                                                                   "here" :kind :error))
                                                               :format-control "~A"
                                                               :format-arguments (list condition))))))
                                     (var:value-cons key (cdr (first cells))))))
                     (when-let ((old-location (location-of (first cells))))
                       (setf (location-of new-cell) old-location))
                     new-cell))))

;;; Uniqueness check

(defun make-uniqueness-checker (format-control &key (test #'equal))
  (let ((seen (make-hash-table :test test)))
    (lambda (key &optional (value nil value-supplied?))
      (cond (value-supplied?
             (setf (gethash key seen) value))
            (t
             (when-let ((previous (gethash key seen)))
               (object-error
                (list (list previous "initial definition"   :note)
                      (list key      "offending definition" :error))
                format-control key)))))))

(defun call-with-uniqueness-check (thunk checker key
                                   &key (value nil value-supplied?))
  (funcall checker key)
  (let ((result (funcall thunk)))
    (funcall checker key (if value-supplied? value result))
    result))

(defmacro with-uniqueness-check ((checker key
                                  &optional (value nil value-supplied?))
                                 &body body)
  `(call-with-uniqueness-check
    (lambda () ,@body) ,checker ,key
    ,@(when value-supplied? `(:value ,value))))
