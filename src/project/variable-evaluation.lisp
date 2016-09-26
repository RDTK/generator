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
              ((list (or :ref :ref/list) (optima:guard pattern (stringp pattern))
                     :default default)
               (recur (lookup pattern default)))

              ((list (or :ref :ref/list) (optima:guard pattern (stringp pattern)))
               (recur (lookup pattern)))

              ((list* :ref pattern rest)
               (recur (list* :ref (first (recur pattern)) rest)))

              ((list* :ref/list pattern rest)
               (first (recur (list* :ref/list (first (recur pattern)) rest))))

              ((optima:guard pattern (atom pattern))
               (list pattern))

              ((list* :list subpatterns)
               (list (mappend #'recur subpatterns)))

              ((list* :alist subpatterns)
               (list (mapcar (lambda+ ((key . value))
                               (cons key (first (recur value))))
                             subpatterns)))

              ((list* first rest)
               (let ((result (mappend #'recur (cons first rest))))
                 (list (collapse result))))))))
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
