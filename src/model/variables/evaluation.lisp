;;;; evaluation.lisp --- Evaluation of value expressions.
;;;;
;;;; Copyright (C) 2012-2019 Jan Moringen
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
  (with-augmented-trace ((list :drill-down path) value (cons :unused value))
    (reduce (lambda (key value)
              ;; (check-type value (or null variable-expression-alist))
              (cdr (assoc key value)))
            path :initial-value value :from-end t)))

(defun drill-down/raw (path value)
  (with-augmented-trace ((list :drill-down/raw path) value (cons :unused value))
    (reduce (lambda (key value)
              (check-type value (or null variable-expression-alist))
              (cdr (assoc key (cdr value))))
            path :initial-value value :from-end t)))

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

(defun generate-arguments (expressions evaluate)
  (let+ ((expressions expressions)
         (arguments   '())
         ((&flet fetch ()
            (when expressions
              (setf arguments (funcall evaluate (pop expressions))))))
         ((&flet skip ()
            (cond (arguments
                   (pop arguments)
                   t)
                  ((typep (first expressions) '(cons (member :ref/list :call/list)))
                   (fetch)
                   (if arguments
                       (progn
                         (pop arguments)
                         t)
                       nil))
                  (t
                   (if expressions
                       (progn
                         (pop expressions)
                         t)
                       nil)
                   ))))
         ((&flet argument ()
            (unless arguments
              (fetch))
            (if arguments
                (values (pop arguments) t)
                (values nil             nil)))))
    (values #'argument #'skip)))

(defun list-arguments (next)
  (loop :for (argument argument?) = (multiple-value-list (funcall next))
        :while argument?
        :collect argument))

(defun expand (pattern lookup)
  (check-type pattern variable-expression)
  (let+ (((&flet lookup (fun)
            (lambda (name &optional (default nil default-supplied?))
              (unless (stringp name)
                (error "Function or variable name is not a string: ~S" name))
              (let ((name (make-keyword (string-upcase name))))
                (if default-supplied?
                    (funcall fun name default)
                    (funcall fun name))))))
         ((&labels recur (pattern lookup path)
            (optima:ematch pattern
              ;; Make the current path available.
              ((list (or :ref :ref/list) "structure-path")
               (list (with-augmented-trace (:structure-path nil)
                       (nreverse (mapcar #'string-downcase (rest path))))))

              ;; Variable reference with already-evaluated variable
              ;; name and with or without default.
              ((list* (or :ref :ref/list) (and name (type string))
                      (or '() (list (and :default default?) default)))
               (let+ (((&values result new-lookup) (if default?
                                                       (funcall lookup name default)
                                                       (funcall lookup name))))
                 (if (string= name "next-value")
                     (when-let ((value (with-augmented-trace (:next-value nil (list result))
                                         (recur result (lookup new-lookup) '()))))
                       (list (drill-down path (first value))))
                     (with-augmented-trace (name nil (cons name result))
                       (recur result (lookup new-lookup) '())))))

              ;; Scalar or list variable reference with to-be-evaluated
              ;; variable name (with or without default).
              ((list* (and (or :ref :ref/list) which) pattern rest)
               (let* ((name     (first (recur pattern lookup path)))
                      (resolved (list* which name rest))
                      (result   (recur resolved lookup path)))
                 (ecase which
                   (:ref      result)
                   (:ref/list (first result)))))

              ((list* (and (or :call :call/list) which) arguments) ; TODO must not be (:call)
               (with-augmented-trace (which nil pattern)
                 (let+ (((&values next skip) (generate-arguments
                                              arguments (rcurry #'recur lookup path)))
                        (designator (with-augmented-trace (:callable nil (list :callable))
                                      (funcall next)))
                        (callable   (etypecase designator
                                      (string (with-augmented-trace (:callable/2 nil (list designator))
                                                (funcall lookup designator)))
                                      (function designator)))
                        ((&values result/raw path)
                         (etypecase callable
                           (function
                            (values (funcall callable arguments next skip) '()))
                           (variable-expression-alist
                            (let ((path (reverse (mapcar (compose #'make-keyword
                                                                  #'string-upcase)
                                                         (list-arguments next)))))
                              (values (drill-down/raw path callable)
                                      path))))))
                   (ecase which
                     (:call      (list result/raw))
                     (:call/list result/raw)))))

              ;; Atomic value.
              ((optima:guard pattern (atom pattern)) ; TODO tighten
               (list pattern))

              ;; List expression.
              ((list* :list subpatterns)
               (list (mappend (rcurry #'recur lookup path) subpatterns)))

              ;; Alist expression
              ((list* :alist subpatterns)
               (list (mapcar (lambda+ ((key . value))
                               (cons key (first (recur value lookup (list* key path)))))
                             subpatterns)))

              ;; Concatenation expression.
              ((list* subpatterns)
               (list (collapse (mappend (rcurry #'recur lookup path) subpatterns))))))))
    (first (recur pattern (lookup lookup) '()))))

(defmethod value ((thing t) (name t) &optional (default nil default-supplied?))
  (let+ (((&values raw raw/next-values defined?)
          (lookup thing name
                  :if-undefined (unless default-supplied? #'error)))
         ((&labels+ make-lookup ((&optional first-value &rest next-values))
            (lambda (name1 &optional (default nil default-supplied?))
              (cond
                ((not (eq name1 :next-value))
                 (let+ (((&values raw raw/next-values defined?)
                         (lookup thing name1
                                 :if-undefined (if default-supplied? (cons :default default) #'error))))
                   (values (cdr raw) (make-lookup raw/next-values) (not defined?))
                   #+no (if default-supplied?
                            (value thing name1 default)
                            (value thing name1))))
                (first-value
                 (progn ; with-augmented-trace (name1 nil first-value)
                   (values (cdr first-value) (make-lookup next-values))))
                (default-supplied?
                 (check-type default (not function))
                 (with-augmented-trace (name1 :default (cons :unused default))
                   (values default (make-lookup next-values) t)))
                (t
                 (error "~@<No next value for ~A.~@:>"
                        name)))))))
    (if defined?
        (with-augmented-trace (name thing raw)
          (with-expansion-stack (name thing)
            (expand (cdr raw) (make-lookup raw/next-values))))
        (progn
          (check-type default (not function))
          (values default t)))))

(defmethod evaluate ((thing t) (expression t))
  (let+ (((&flet lookup (name &optional (default nil default-supplied?))
            (value-parse ; TODO value-parse is a hack
             (if default-supplied?
                 (value thing name default)
                 (value thing name))))))
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
