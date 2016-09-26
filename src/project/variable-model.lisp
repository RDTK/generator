;;;; variable-model.lisp --- Model for value expressions.
;;;;
;;;; Copyright (C) 2012, 2013, 2014, 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.project)

;;; Variable expression functions

(defun value-parse (raw)
  (labels ((rec (thing)
             (cond
               ((stringp thing)
                (let ((result (esrap:parse 'expr thing)))
                  (if (length= 1 result)
                      (first result)
                      result)))
               ((typep thing '(or real boolean))
                thing)
               ((every (of-type '(cons keyword t)) thing)
                `(:alist ,@(mapcar (lambda (cell)
                                     (cons (car cell) (rec (cdr cell))))
                                   thing)))
               (t
                `(:list ,@(mapcar #'rec thing))))))
    (rec raw)))

(defun value-list (&rest elements)
  (mapcar #'value-parse elements))

(defun value-list* (&rest args)
  (let ((values (butlast args))
        (rest (lastcar args)))
    (append (mapcar #'value-parse values) rest)))

(defun value-cons (name value)
  (cons name (value-parse value)))

(defun value-acons (&rest args)
  (let ((cells (butlast args))
        (rest (lastcar args)))
    (append (loop :for (name value) :on cells :by #'cddr
               :collect (cons name (value-parse value)))
            rest)))
