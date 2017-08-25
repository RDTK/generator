;;;; value-types.lisp --- Option value types used in the commands module.
;;;;
;;;; Copyright (C) 2017, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.project.commands)

;;; `recipe-and-version'

(deftype recipe-and-version ()
  `(cons pathname string))
(setf (get 'recipe-and-version 'configuration.options::dont-expand) t)

(defmethod configuration.options:raw->value-using-type
    ((schema-item t)
     (raw         string)
     (type        (eql 'recipe-and-version))
     &key inner-type)
  (declare (ignore inner-type))
  (if-let ((position (position #\@ raw)))
    (cons (pathname (subseq raw 0 position)) (subseq raw (1+ position)))
    (error "~@<Could not parse \"~A\" as a pair of the form ~
            RECIPE@VERSION.~@:>"
           raw)))

(defmethod configuration.options:value->string-using-type
    ((schema-item t)
     (value       cons)
     (type        (eql 'recipe-and-version))
     &key inner-type)
  (declare (ignore inner-type))
  (let+ (((recipe . version) value))
    (format nil "~A@~A" recipe version)))

;;; `variable-assignment'

(deftype variable-assignment ()
  `(cons keyword t))
(setf (get 'variable-assignment 'configuration.options::dont-expand) t)

(defmethod configuration.options:raw->value-using-type
    ((schema-item t)
     (raw         string)
     (type        (eql 'variable-assignment))
     &key inner-type)
  (declare (ignore inner-type))
  (let+ ((position  (or (position #\= raw)
                        (error "~@<Variable assignment ~S is not of the ~
                                form NAME=VALUE.~@:>"
                               raw)))
         (name/raw  (subseq raw 0 position))
         (name      (make-keyword (string-upcase name/raw)))
         (value/raw (subseq raw (1+ position)))
         (value     (if (and (not (emptyp value/raw))
                             (let ((first (aref value/raw 0)))
                               (or (member first '(#\" #\{ #\[))
                                   (digit-char-p first))))
                        (let ((json:*json-identifier-name-to-lisp* #'string-upcase))
                          (json:decode-json-from-string value/raw))
                        value/raw)))
    (cons name value)))

(defmethod configuration.options:value->string-using-type
    ((schema-item t)
     (value       cons)
     (type        (eql 'variable-assignment))
     &key inner-type)
  (declare (ignore inner-type))
  (let+ (((variable . value) value)
         (value/json (typecase value
                       (string
                        value)
                       (t
                        (let ((json:*lisp-identifier-name-to-json*
                               #'string-downcase))
                          (json:encode-json-to-string value))))))
    (format nil "~(~A~)=~A" variable value/json)))
