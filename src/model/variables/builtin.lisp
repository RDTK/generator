;;;; builtin.lisp --- Builtin functions.
;;;;
;;;; Copyright (C) 2012-2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.model.variables)

;;; Utilities

(defun call (function &rest arguments)
  (let+ ((remaining arguments)
         ((&flet next ()
            (if remaining
                (values (pop remaining) t)
                (values nil             nil))))
         ((&flet skip ()
            (pop remaining))))
    (funcall function arguments #'next #'skip)))

;;; Registry

(defvar *builtin-operators* '())

(defun builtin-operator (name)
  (assoc-value *builtin-operators* name))

(defun (setf builtin-operator) (new-value name)
  (setf (assoc-value *builtin-operators* name) new-value))

(defmacro define-builtin-operator (name
     (next-var
      &optional
      (skip-var (gensym "SKIP") skip-var-supplied?)
      (done-var (gensym "DONE") done-var-supplied?))
     &body body)
  (with-gensyms (arguments-var index-var exhausted?-var)
    `(progn
       (setf (builtin-operator ',name)
             (lambda (,arguments-var ,next-var ,skip-var)
               (declare (ignore ,arguments-var
                                ,@(unless skip-var-supplied?
                                    `(,skip-var))))
               (let ((,index-var 0)
                     ,@(when done-var-supplied?
                         `((,exhausted?-var nil))))
                 (flet ((,next-var (&optional (optional? nil))
                            (multiple-value-prog1
                                (multiple-value-bind (value value?)
                                    (funcall ,next-var)
                                  ,@(when done-var-supplied?
                                      `((setf ,exhausted?-var (not value?))))
                                  (when (and (not value?) (not optional?))
                                    (error "The operator ~S requires at least ~D argument~:P but only ~D ~:*~[have~;has~:;have~] been supplied."
                                           ',name (1+ ,index-var) ,index-var))
                                  (values value value?))
                              (incf ,index-var)))
                        ,@(when skip-var-supplied?
                            `((,skip-var ()
                                (if (funcall ,skip-var)
                                    (incf ,index-var)
                                    (setf ,exhausted?-var t)))))
                        ,@(when done-var-supplied?
                            `((,done-var ()
                                (unless (or ,exhausted?-var
                                            (setf ,exhausted?-var (not (funcall ,skip-var))))
                                  (error "The operator ~S accepts at most ~D argument~:P but ~D ~:*~[have~;has~:;have~] been supplied."
                                         ',name ,index-var (1+ ,index-var)))))))
                   ,@body))))
       ',name)))

(defmacro define-builtin-function (name lambda-list &body body)
  (with-gensyms (next-var)
    `(define-builtin-operator ,name (,next-var)
       (destructuring-bind ,lambda-list (list-arguments ,next-var)
         ,@body))))

;;; Logical operators

(define-builtin-operator :not (next skip done)
  (if (prog1
          (as (next) 'boolean :if-type-mismatch 1)
        (done))
      nil
      t))

(define-builtin-operator :and (next)
  (loop :for     previous             = nil :then argument
        :for     (argument argument?) = (multiple-value-list (next))
        :while   argument?
        :always  (as argument 'boolean :if-type-mismatch 1)
        :finally (return previous)))

;; (operator-and)
;; (operator-and (lambda () (print 1) (list 5)))
;; (operator-and (lambda () (print 2) (list t nil)))
;; (operator-and (lambda () (print 2) (list nil)))
;; (operator-and (lambda () (print 1) (list nil)) (lambda () (print 2) (list nil)))
;; (operator-and (lambda () (print 3) (list t t t))   (lambda () (print 4) (list t nil)))
;; (operator-and (lambda () (print 5) (list nil)) (lambda () (print 5) (list t)))

(define-builtin-operator :or (next)
  (loop :for    (argument argument?) = (multiple-value-list (next t))
        :while  argument?
        :when   (as argument 'boolean :if-type-mismatch 1)
        :return argument))

(define-builtin-operator :if (next skip done)
  (prog1
      (if (as (next) 'boolean :if-type-mismatch 1)
          (prog1 (next) (skip))
          (progn (skip) (next t)))
    (done)))

(setf (builtin-operator :fn)
      (lambda (arguments next skip)
        (declare (ignore next skip))
        (destructuring-bind (operator (keyword . lambda-list) &rest body) arguments
          (declare (ignore operator keyword))
          (lambda (arguments next skip)
            (declare (ignore arguments skip))
            (let ((environment (make-instance 'direct-variables-mixin
                                              :variables (loop :for name  :in lambda-list
                                                               :for value =   (funcall next)
                                                               :collect (cons (make-keyword (string-upcase name)) value)))))
              (describe environment *trace-output*)
              (evaluate environment body))))))

;;; Ordinary functions

(define-builtin-function :equal (left right)
  (equal left right))

(define-builtin-function :+ (number &rest numbers)
  (apply #'+ number numbers))

(define-builtin-function :upcase (string)
  (string-upcase string))

(define-builtin-function :downcase (string)
  (string-downcase string))

(define-builtin-function :capitalize (string)
  (string-capitalize string))

(define-builtin-function :map (function &rest sequences)
  (let ((r (apply #'map 'list (curry #'call function) sequences)))
    (log:error :map r)
    r))
