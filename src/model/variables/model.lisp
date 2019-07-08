;;;; model.lisp --- Model for value expressions.
;;;;
;;;; Copyright (C) 2012-2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:build-generator.model.variables)

;;; Values
;;;
;;; Values that evaluating a "variable expression" can result
;;; in. Basically atomic values of type `string', `real' or `boolean'
;;; as well as lists and alists of those:
;;;
;;; value        ::= atomic-value
;;;                  (value*)
;;;                  ((key . value)*)
;;;
;;; atomic-value ::= string
;;;                  real
;;;                  boolean

(deftype variable-expression-value ()
  `(or atomic-variable-expression-value
       (satisfies variable-expression-value-list?)
       (satisfies variable-expression-value-alist?)))

(deftype atomic-variable-expression-value ()
  `(or string real boolean))

(defun variable-expression-value-list? (thing)
  (and (listp thing)
       (every (of-type 'variable-expression-value) thing)))

(deftype variable-expression-value-alist-element ()
  `(cons keyword variable-expression-value))

(defun variable-expression-value-alist? (thing)
  (and (listp thing)
       (every (of-type 'variable-expression-value-alist-element) thing)))

;;; Unevaluated Expressions
;;;
;;; In addition to atomic values (of type `string', `real' or
;;; `boolean') and compound list, alist and concatenation expressions,
;;; these expressions can contain several kinds of variable
;;; references.
;;;
;;; expression         ::= atomic-value
;;;                        variable-reference
;;;                        concatenation
;;;                        list
;;;                        alist
;;;
;;; atomic-value       ::= string
;;;                        real
;;;                        boolean
;;;
;;; variable-reference ::= (:ref      expression [:default expression])
;;;                        (:ref/list expression [:default expression])
;;;
;;; concatenation      ::= (expression*)
;;;
;;; list               ::= (:list expression*)
;;;
;;; alist              ::= (:alist alist-element*)
;;;
;;; alist-element      ::= (key . expression)

(deftype variable-expression ()
  `(or atomic-variable-expression-value
       variable-expression-concatenation
       variable-expression-list
       variable-expression-alist
       variable-reference))

(defun variable-expression? (thing)
  (typep thing 'variable-expression))

(deftype variable-reference ()
  `(cons (member :ref :ref/list)
         (cons (satisfies variable-expression?)                 ; = variable-expression
               (or null
                   (cons (eql :default)
                         (cons (satisfies variable-expression?) ; = variable-expression
                               null))))))

(defun variable-expression-concatenation? (thing)
  (and (consp thing)
       (every (of-type '(or atomic-variable-expression-value
                            variable-expression-concatenation
                            variable-reference))
              thing)))

(deftype variable-expression-concatenation ()
  `(satisfies variable-expression-concatenation?))

(defun variable-expression-list? (thing)
  (and (typep thing '(cons (eql :list) list))
       (every (of-type 'variable-expression) (rest thing))))

(deftype variable-expression-list ()
  `(satisfies variable-expression-list?))

(defun variable-expression-list-element-list? (thing)
  (and (listp thing)
       (every (of-type 'variable-expression) thing)))

(deftype variable-expression-list-element-list ()
  `(satisfies variable-expression-list-element-list?))

(defun variable-expression-alist-element? (thing)
  (and (consp thing)
       (keywordp (car thing))
       (typep (cdr thing) 'variable-expression)))

(defun variable-expression-alist? (thing)
  (and (typep thing '(cons (eql :alist) list))
       (every #'variable-expression-alist-element? (rest thing))))

(deftype variable-expression-alist ()
  `(satisfies variable-expression-alist?))

(deftype variable-expression-alist-element ()
  `(satisfies variable-expression-alist-element?))

(defun variable-expression-alist-element-list? (thing)
  (and (listp thing)
       (every #'variable-expression-alist-element? thing)))

(deftype variable-expression-alist-element-list ()
  `(satisfies variable-expression-alist-element-list?))

;;; Variable expression functions

(defun value-parse (raw)
  (labels ((rec (thing)
             (cond
               ((typep thing '(or real boolean))
                thing)
               ((and (stringp thing) (emptyp thing))
                nil)
               ((and (stringp thing)
                     (not (position #\{ thing))
                     (not (position #\\ thing)))
                thing)
               ((stringp thing)
                (let ((result (esrap:parse 'expr thing)))
                  (if (length= 1 result)
                      (first result)
                      result)))
               ((not (consp thing))
                (error "Invalid value: ~S" thing))
               ((every (of-type '(cons keyword t)) thing)
                `(:alist ,@(mapcar (lambda (cell)
                                     (cons (car cell) (rec (cdr cell))))
                                   thing)))
               (t
                `(:list ,@(mapcar #'rec thing))))))
    (rec raw)))

(defun value-unparse (value)
  (labels ((rec (expr)
             (optima:match expr
               ((type (or real boolean))
                expr)

               ((type string)
                (ppcre:regex-replace "\\${" expr "\\${"))

               ((list* (and kind (or :ref :ref/list))
                       name (or nil (list :default default)))
                (format nil "~C{~A~@[|~A~]}"
                        (case kind
                          (:ref      #\$)
                          (:ref/list #\@))
                        (rec name)
                        (when default (rec default))))

               ((list* :list elements)
                (map 'list #'rec elements))

               ((list* :alist elements)
                (map 'list (lambda+ ((car . cdr))
                             (cons car (rec cdr)))
                     elements))

               ((list* elements)
                (format nil "~{~A~}" (map 'list #'rec elements))))))
    (rec value)))

(defun value-list (&rest elements)
  (mapcar #'value-parse elements))

(defun value-list* (&rest args)
  (let ((values (butlast args))
        (rest (lastcar args)))
    (check-type rest variable-expression-list-element-list)
    (append (mapcar #'value-parse values) rest)))

(defun value-cons (name value)
  (check-type name keyword)
  (cons name (value-parse value)))

(defun value-acons (&rest args)
  (let ((cells (butlast args))
        (rest  (lastcar args)))
    (check-type rest variable-expression-alist-element-list)
    (append (loop :for (name value) :on cells :by #'cddr
               :do (check-type name keyword)
               :collect (cons name (value-parse value)))
            rest)))

(defun to-value (thing)
  (typecase thing
    ((or real boolean string)
     thing)
    ((cons (cons keyword))
     (list* (cons (car (first thing))
                  (cdr (to-value (first thing))))
            (to-value (rest thing))))
    (cons
     (map 'list #'to-value thing))
    (t
     (princ-to-string thing))))
