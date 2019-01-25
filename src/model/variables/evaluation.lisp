;;;; evaluation.lisp --- Evaluation of value expressions.
;;;;
;;;; Copyright (C) 2012-2018 Jan Moringen
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

(cl:in-package #:jenkins.model.variables)

;;; Merging lookup results

(defun+ merge-lookup-results ((left-value  left-more  left-defined?)
                              (right-value right-more right-defined?))
  (cond
    ((and left-defined? right-defined?)
     (list left-value (append left-more (list right-value) right-more) t))
    (left-defined?
     (list left-value left-more t))
    (right-defined?
     (list right-value right-more t))
    (t
     '(nil () nil))))

(defun merge-lookup-values (left-value  left-more  left-defined?
                            right-value right-more right-defined?)
  (cond
    ((and left-defined? right-defined?)
     (values left-value (append left-more (list right-value) right-more) t))
    (left-defined?
     (values left-value left-more t))
    (right-defined?
     (values right-value right-more t))
    (t
     (values nil () nil))))

;;;

(defvar *stack* '())

(defun call-with-expansion-stack (thunk name thing)
  (let ((key (list name thing)))
    (when (find key *stack* :test #'equal)
      (error 'expression-cycle-error
             :path (reverse (list* key *stack*))))
    (let ((*stack* (list* key *stack*)))
      (funcall thunk))))

(defmacro with-expansion-stack ((name thing) &body body)
  `(call-with-expansion-stack (lambda () ,@body) ,name ,thing))

(defun drill-down (path value)
  (reduce (lambda (key value)
            (cdr (assoc key value)))
          path :initial-value value :from-end t))

(defun collapse (thing)
  (let+ (((&flet atom? (thing)
            (typep thing '(or number string (eql t))))))
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

(defun expand (pattern lookup)
  (check-type pattern variable-expression)
  (let+ (((&flet lookup (name &optional (default nil default-supplied?))
            (let ((name (make-keyword (string-upcase name))))
              (if default-supplied?
                  (funcall lookup name default)
                  (funcall lookup name)))))
         ((&labels recur (pattern path)
            (optima:ematch pattern
              ;; Make the current path available.
              ((list (or :ref :ref/list) "structure-path")
               (list (with-augmented-trace (:structure-path nil)
                       (nreverse (mapcar #'string-downcase (rest path))))))

              ;; Variable reference with already-evaluated variable
              ;; name and with or without default.
              ((list* (or :ref :ref/list) (and name (type string))
                      (or '() (list (and :default default?) default)))
               (let ((result (if default?
                                 (lookup name (lambda ()
                                                (first (recur default path))))
                                 (lookup name))))
                 (list (if (string= name "next-value")
                           (drill-down path result)
                           result))))

              ;; Scalar or list variable reference with to-be-evaluated
              ;; variable name (with or without default).
              ((list* (and (or :ref :ref/list) which) pattern rest)
               (let* ((name     (first (recur pattern path)))
                      (resolved (list* which name rest))
                      (result   (recur resolved path)))
                 (ecase which
                   (:ref      result)
                   (:ref/list (first result)))))

              ;; Atomic value.
              ((optima:guard pattern (atom pattern)) ; TODO tighten
               (list pattern))

              ;; List expression.
              ((list* :list subpatterns)
               (list (mappend (rcurry #'recur path) subpatterns)))

              ;; Alist expression
              ((list* :alist subpatterns)
               (list (mapcar (lambda+ ((key . value))
                               (cons key (first (recur value (list* key path)))))
                             subpatterns)))

              ;; Concatenation expression.
              ((list* subpatterns)
               (list (collapse (mappend (rcurry #'recur path) subpatterns))))))))
    (first (recur pattern '()))))

(defmethod value ((thing t) (name t) &optional (default nil default-supplied?))
  (let+ (((&values raw raw/next-values defined?)
          (lookup thing name
                  :if-undefined (unless default-supplied? #'error)))
         ((&labels+ make-lookup ((&optional first-value &rest next-values))
            (lambda (name1 &optional (default nil default-supplied?))
              (cond
                ((not (eq name1 :next-value))
                 (if default-supplied?
                     (value thing name1 default)
                     (value thing name1)))
                (first-value
                 (with-augmented-trace (name1 nil first-value)
                   (expand (cdr first-value) (make-lookup next-values))))
                (default-supplied?
                 (with-augmented-trace (name1 :default (cons :unused default))
                   (if (functionp default) (funcall default) default)))
                (t
                 (error "~@<No next value for ~A.~@:>"
                        name)))))))
    (if defined?
        (with-augmented-trace (name thing raw)
          (with-expansion-stack (name thing)
            (expand (cdr raw) (make-lookup raw/next-values))))
        (values (if (functionp default) (funcall default) default) t)))) ; TODO function business

(defmethod evaluate ((thing t) (expression t))
  (let+ (((&flet lookup (name &optional (default nil default-supplied?))
            (if default-supplied?
                (value thing name default)
                (value thing name)))))
    (expand expression #'lookup)))

;;; Casts

(defmethod as ((value t) (type (eql 'boolean)) &key if-type-mismatch)
  (declare (ignore if-type-mismatch))
  (switch (value :test #'equal)
    ('t      (values t   t))
    ("true"  (values t   t))
    (nil     (values nil t))
    ("false" (values nil t))))

(defmethod as ((value string) (type (eql 'keyword)) &key if-type-mismatch)
  (declare (ignore if-type-mismatch))
  (values (make-keyword (string-upcase value)) t))

(defmethod as ((value t) (type cons) &key if-type-mismatch)
  (declare (ignore if-type-mismatch))
  (case (first type)
    (list-of
     (let ((element-type (second type)))
       (values-list
        (reduce (lambda+ (value (values all-match?))
                  (let+ (((&values value match?)
                          (as value element-type
                              :if-type-mismatch nil)))
                    (list (list* value values)
                          (and match? all-match?))))
                value :initial-value '(() t) :from-end t))))
    (or
     (dolist (type (rest type))
       (let+ (((&values value match?)
               (as value type :if-type-mismatch nil)))
         (when match?
           (return-from as (values value t))))))
    (eql
     (let+ ((expected (second type))
            ((&flet convert ()
               (let+ ((type (typecase expected
                              (keyword 'keyword)
                              (t       (class-name (class-of expected)))))
                      ((&values value match?) (as value type
                                                  :if-type-mismatch nil)))
                 (when (and match? (eql value expected))
                   (values value t))))))
       (if (eql value expected)
           (values value t)
           (convert))))
    (t
     (call-next-method))))

(defmethod value/cast ((thing t) (name symbol) &optional (default nil default-supplied?))
  (let+ (((&values value defaulted?)
          (apply #'value thing name
                 (when default-supplied? (list default))))
         (variable (find-variable name :if-does-not-exist #'error)))
    (values (as value (variable-info-type variable)) defaulted?)))
