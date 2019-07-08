;;;; grammar.lisp --- Grammar for value expressions.
;;;;
;;;; Copyright (C) 2012-2017, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:build-generator.model.variables)

(defun maybe-first (thing)
  (if (and (length= 1 thing)
           (typep (first thing) '(or string (cons (member :ref :ref/list)))))
      (first thing)
      thing))

(esrap:defrule escaped-syntactic-character
    (and #\\ (or #\$ #\@ #\}))
  (:function second))

(esrap:defrule uninterpreted-$-or-@
    (and (or #\$ #\@) (esrap:! #\{))
  (:function first))

(esrap:defrule text
    (+ (or escaped-syntactic-character
           uninterpreted-$-or-@
           (not (or #\$ #\@))))
  (:text t))

(esrap:defrule variable-reference/content
    (+ (not (or #\| #\} #\$ #\@)))
  (:text t))

(esrap:defrule text/ended-by-}
    (and (+ (or escaped-syntactic-character (not (or #\$ #\@ #\}))))
         (esrap:& #\}))
  (:function first)
  (:text t))

(esrap:defrule text/not-started-by-{
    (and (esrap:! #\}) text)
  (:function second))

(esrap:defrule reference-expr
    (+ (or variable-reference/content variable-reference)))

(esrap:defrule default-expr
    (and (+ (or variable-reference text/ended-by-} text/not-started-by-{))
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

(esrap:defrule expr
    (* (or variable-reference text)))
