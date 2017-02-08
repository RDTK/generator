;;;; grammar.lisp --- Grammar for value expressions.
;;;;
;;;; Copyright (C) 2012-2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.model.variables)

(defun maybe-first (thing)
  (if (and (length= 1 thing)
           (typep (first thing) '(or string (cons (member :ref :ref/list :call :call/list)))))
      (first thing)
      thing))

(esrap:defrule escaped-syntactic-character
    (and #\\ (or #\$ #\@ #\}))
  (:function second))

(esrap:defrule uninterpreted-$-or-@
    (and (or #\$ #\@) (esrap:! (or #\{ #\()))
  (:function first))

(esrap:defrule text-character
    (or escaped-syntactic-character
        uninterpreted-$-or-@
        (not (or #\$ #\@))))

(esrap:defrule text
    (+ text-character)
  (:text t))

(esrap:defrule reference-expr/content
    (+ (and (esrap:! (or #\| #\})) text-character))
  (:text t))

(esrap:defrule reference-expr
    (+ (or reference-expr/content
           variable-reference
           function-call)))

(esrap:defrule default-expr
    (and (+ (or reference-expr/content
                variable-reference
                function-call))
         (esrap:& #\}))
  (:function first)
  (:function maybe-first))

(esrap:defrule variable-reference
    (and (or #\$ #\@) #\{
         reference-expr (esrap:? (and #\| (esrap:? default-expr)))
         #\})
  (:destructure (kind open content default close)
    (declare (ignore open close))
    (let ((default (when default
                     (list :default (unless (equal (second default) "[]")
                                      (second default))))))
      (cond
        ((string= kind "$")
         (list* :ref content default))
        ((string= kind "@")
         (list* :ref/list content default))))))

(esrap:defrule argument-expr/escaped-syntactic-character
    (and #\\ (or #\Space #\)))
  (:function second))

(esrap:defrule argument-expr/content
    (+ (and (esrap:! (or #\Space #\)))
            (or argument-expr/escaped-syntactic-character
                text-character)))
  (:text t))

(esrap:defrule argument-expr
    (+ (or argument-expr/content
           variable-reference
           function-call))
  (:function maybe-first))

(esrap:defrule arguments
    (and argument-expr (* (and (+ #\Space) argument-expr)))
  (:destructure (first rest)
    (list* first (mapcar #'second rest))))

(esrap:defrule function-call
    (and (or #\$ #\@) #\( arguments #\))
  (:destructure (kind open arguments close)
    (declare (ignore open close))
    (cond
      ((string= kind "$")
       (list* :call      arguments))
      ((string= kind "@")
       (list* :call/list arguments)))))

(esrap:defrule expr
    (* (or variable-reference function-call text)))
