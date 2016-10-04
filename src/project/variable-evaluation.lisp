;;;; variable-evaluation.lisp --- Evaluation of value expressions.
;;;;
;;;; Copyright (C) 2012, 2013, 2014, 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

;;;; In this context, evaluation translates to computing the effective
;;;; value of variables which mainly consists of evaluating "variable
;;;; expressions" in the context of a set of variable bindings.
;;;;
;;;; The following functions are involved:
;;;;
;;;; value : context, name -> value
;;;;
;;;;   The function `value' performs such an effective value
;;;;   computation by coordinating the `lookup' and `expand' function.
;;;;
;;;; lookup : context, name -> variable expression, variable expressions
;;;;
;;;;   `lookup' computes the value of a variable given a set of
;;;;   variable bindings.
;;;;
;;;; expand : variable expression, lookup function -> value
;;;;
;;;;   `expand' computes the value of a "variable expression" relying
;;;;   on `lookup' to resolve variable references.

(cl:in-package #:jenkins.project)

(defmethod lookup ((thing t) (name t)
                   &key
                   if-undefined)
  (declare (ignore if-undefined))
  (when-let ((cells (remove name (variables thing)
                            :test (complement #'eq)
                            :key  #'car)))
    (values (first cells) (rest cells) t)))

(defun expand (pattern lookup)
  (let+ (((&flet lookup (name &optional (default nil default-supplied?))
            (let+ ((name (make-keyword (string-upcase name)))
                   ((&values value parsed? defaulted?)
                    (if default-supplied?
                        (funcall lookup name default)
                        (funcall lookup name))))
              (if defaulted?
                  value
                  (parse value :parse-strings? (not parsed?))))))
         ((&flet atom? (thing)
            (typep thing '(or number string (eql t)))))
         ((&labels collapse (thing)
            (cond
              ((or (atom? thing) (eq thing nil))
               thing)
              ((every #'atom? thing)
               (esrap:text (mapcar #'princ-to-string thing)))
              ((every #'listp thing)
               (reduce #'append thing))
              (t
               (apply #'map-product (compose #'collapse #'list)
                      (mapcar #'ensure-list thing))))))
         ((&labels recur (pattern)
            (optima:ematch pattern
              ;; Variable reference with already-evaluated variable
              ;; name and with default.
              ((list (or :ref :ref/list) (optima:guard pattern (stringp pattern))
                     :default default)
               (recur (lookup pattern default)))

              ;; Variable reference with already-evaluated variable
              ;; name and without default.
              ((list (or :ref :ref/list) (optima:guard pattern (stringp pattern)))
               (recur (lookup pattern)))

              ;; Scalar variable reference with to-be-evaluated
              ;; variable name (with or without default).
              ((list* :ref pattern rest)
               (recur (list* :ref (first (recur pattern)) rest)))

              ;; List variable reference with to-be-evaluated variable
              ;; name (with or without default.
              ((list* :ref/list pattern rest)
               (first (recur (list* :ref/list (first (recur pattern)) rest))))

              ;; Atomic value.
              ((optima:guard pattern (atom pattern))
               (list pattern))

              ;; List expression.
              ((list* :list subpatterns)
               (list (mappend #'recur subpatterns)))

              ;; Alist expression
              ((list* :alist subpatterns)
               (list (mapcar (lambda+ ((key . value))
                               (cons key (first (recur value))))
                             subpatterns)))

              ;; Concatenation expression.
              ((list* subpatterns)
               (list (collapse (mappend #'recur subpatterns))))))))
    (first (recur pattern))))

(defmethod value ((thing t) (name t) &optional (default nil default-supplied?))
  (let+ (((&values raw raw/next-values defined?)
          (lookup thing name
                  :if-undefined (unless default-supplied? #'error)))
         ((&labels+ make-lookup ((&optional first-value &rest next-values))
            (lambda (name1 &optional (default nil default-supplied?))
              (cond
                ((not (eq name1 :next-value))
                 (if default-supplied?
                     (let+ (((&values value defaulted?) (value thing name1 default)))
                       (values value t defaulted?))
                     (values (value thing name1) t)))
                (first-value
                 (with-augmented-trace (name1 nil first-value)
                   (values (expand (parse (cdr first-value))
                                   (make-lookup next-values))
                           t)))
                (default-supplied?
                 (with-augmented-trace (name1 :default (cons :unused default))
                   (values default t t)))
                (t
                 (error "~@<No next value for ~A.~@:>"
                        name)))))))
    (with-augmented-trace (name thing raw)
      (if defined?
          (expand (parse (cdr raw)) (make-lookup raw/next-values))
          (values default t)))))

;;; Casts

(defmethod as ((value t) (type (eql 'boolean)) &key if-type-mismatch)
  (declare (ignore if-type-mismatch))
  (switch (value :test #'equal)
    ('t      (values t   t))
    ("true"  (values t   t))
    (nil     (values nil t))
    ("false" (values nil t))))

(defmethod as ((value t) (type cons) &key if-type-mismatch)
  (declare (ignore if-type-mismatch))
  (case (car type)
    (or
     (dolist (type (rest type))
       (let+ (((&values value match?)
               (as value type :if-type-mismatch nil)))
         (when match?
           (return-from as (values value t))))))
    (eql
     (when (eql value (second type))
       (values value t)))
    (t
     (call-next-method))))
