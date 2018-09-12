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
    (+ (and (esrap:! (or #\Space #\Newline #\) #\|))
            (or argument-expr/escaped-syntactic-character
                text-character)))
  (:text t))

(defun parse-yaml/block (text position end)
  (let ((start (position #\Space text
                         :test-not #'char=
                         :start    position
                         :end      end)))
    (log:warn position start (- start position))
    (let+ (((&values production position success?)
            (architecture.builder-protocol:with-builder
                ((make-instance 'language.yaml.construct:native-builder))
              (let ((language.yaml.parser::*c* :block-in)
                    (language.yaml.parser::*n* (- start position 1)))
                (esrap:parse 'language.yaml.parser::s-l+block-in-block
                             text :start position :junk-allowed t)))))
      (if success?
          (values (value-parse production) (if position (1- position) (1- end)) t)
          (values nil                      position                             nil)))))

(esrap:defrule yaml/block
    (and (* #\Space) #\Newline #'parse-yaml/block)
  (:function third))

(defun parse-yaml/flow/plain (text position end)
  (let+ ((end (min (or (position #\Space text :start position) end)
                   (or (position #\)     text :start position) end)))
         ((&values production position success?)
          (architecture.builder-protocol:with-builder
              ((make-instance 'language.yaml.construct:native-builder))
            (let ((language.yaml.parser::*c* :flow-in)
                  (language.yaml.parser::*n* -1))
              (esrap:parse 'language.yaml.parser::ns-flow-yaml-node
                           text :start position :end end :junk-allowed t)))))
    (if success?
        (values (value-parse production) (or position end) t)
        (values nil                      (or position end) nil))))

(defun parse-yaml/flow/json (text position end)
  (let+ (((&values production position success?)
          (architecture.builder-protocol:with-builder
              ((make-instance 'language.yaml.construct:native-builder))
            (let ((language.yaml.parser::*c* :flow-in)
                  (language.yaml.parser::*n* -1))
              (esrap:parse 'language.yaml.parser::c-flow-json-node
                           text :start position :junk-allowed t)))))
    (if success?
        (values (value-parse production) position t)
        (values nil                      position nil))))

(esrap:defrule yaml/flow
    (or #'parse-yaml/flow/plain
        #'parse-yaml/flow/json))

(esrap:defrule foo
    (and (* (or #\Space #\Newline))
         (or variable-reference
             function-call
             yaml/flow))
  (:function second))

(esrap:defrule argument-expr
    (or yaml/block foo))

(esrap:defrule arguments
    (* argument-expr))

(esrap:defrule function-call
    (and (or #\$ #\@) #\( arguments (and (* (or #\Space #\Newline)) #\)))
  (:destructure (kind open arguments close)
    (declare (ignore open close))
    (cond
      ((string= kind "$")
       (list* :call      arguments))
      ((string= kind "@")
       (list* :call/list arguments)))))

(esrap:defrule expr
    (* (or variable-reference function-call text)))
