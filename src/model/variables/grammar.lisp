;;;; grammar.lisp --- Grammar for value expressions.
;;;;
;;;; Copyright (C) 2012-2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.model.variables)

(defun maybe-first (thing)
  (if (and (length= 1 thing)
           (typep (first thing) '(or string (cons (member :ref :ref/list :call :call/list)))))
      (first thing)
      thing))

;;; Lexical stuff

(esrap:defrule whitespace
    (or #\Space #\Newline))

(esrap:defrule whitespace+
    (+ (or #\Space #\Newline)))

(esrap:defrule whitespace*
    (* (or #\Space #\Newline)))

(esrap:defrule evaluated-start
    (esrap:character-ranges #\$ #\@)
  (:lambda (character)
    (ecase character
      (#\$ :scalar)
      (#\@ :list))))

(esrap:defrule escaped-syntactic-character
    (and #\\ (or #\$ #\@ #\}))
  (:function second))

(esrap:defrule uninterpreted-evaluated-start
    (and (or #\$ #\@) (esrap:! (or #\{ #\()))
  (:function first))

;;; Text

(esrap:defrule text-character
    (or escaped-syntactic-character
        uninterpreted-evaluated-start
        (not evaluated-start)))

(esrap:defrule text
    (+ text-character)
  (:text t))

;;; Embedded YAML

(macrolet ((parse-yaml (rule context indent &key start end)
             `(let+ (((&values production position success?)
                      (architecture.builder-protocol:with-builder
                          ((make-instance 'language.yaml.construct:native-builder))
                        (let ((language.yaml.parser::*c* ,context)
                              (language.yaml.parser::*n* ,indent))
                          (esrap:parse ',rule
                                       text
                                       :start ,start
                                       ,@(when end `(:end ,end))
                                       :junk-allowed t)))))
                (if success?
                    (values (value-parse production) (if position (1- position) (1- end)) t)
                    (values nil                      position                             nil)))))

  (defun parse-yaml/block (text position end)
    (let ((start (position #\Space text
                           :test-not #'char=
                           :start    position
                           :end      end)))
      #+later (parse-yaml language.yaml.parser::s-l+block-in-block :block-in (- start position 1)
                  :start position)
      (let+ (((&values production position success?)
              (architecture.builder-protocol:with-builder
                  ((make-instance 'language.yaml.construct:native-builder))
                (let ((language.yaml.parser::*c* :block-in)
                      (language.yaml.parser::*n* (- start position 1)))
                  (esrap:parse 'language.yaml.parser::s-l+block-in-block
                               text :start position :junk-allowed t))))
             (position (or position end)))
        (cond ((not success?)
               (values nil position nil))
              ((not (char= (aref text (1- position)) #\Newline))
               (values nil position nil))
              (t
               (values (value-parse production) (1- position) t))))))

  (defun parse-yaml/flow/json (text position end)
    #+later (parse-yaml language.yaml.parser::c-flow-json-node :flow-in -1
                :start position)

    (let+ (((&values production position success?)
            (architecture.builder-protocol:with-builder
                ((make-instance 'language.yaml.construct:native-builder))
              (let ((language.yaml.parser::*c* :flow-in)
                    (language.yaml.parser::*n* -1))
                (esrap:parse 'language.yaml.parser::c-flow-json-node
                             text :start position :end end :junk-allowed t)))))
      (if success?
          (values (value-parse production) position t)
          (values nil                      position nil))))

  (defun parse-yaml/flow/plain (text position end)
    #+later (parse-yaml language.yaml.parser::ns-flow-yaml-node :flow-in -1
                                                        :start position :end end)

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
          (values nil                      (or position end) nil)))))

(esrap:defrule yaml/block
    #'parse-yaml/block)

(esrap:defrule yaml/flow
    (or #'parse-yaml/flow/json
        #'parse-yaml/flow/plain))

;;; Variable reference

(esrap:defrule reference-expr/content
    (+ (and (esrap:! (or #\| #\})) text-character))
  (:text t))

(esrap:defrule reference-expr
    (+ (or reference-expr/content
           variable-reference
           function-call)))

(esrap:defrule default-expr
    (and reference-expr (esrap:& #\}))
  (:function first)
  (:function maybe-first))

(esrap:defrule variable-reference
    (and evaluated-start #\{
         reference-expr (esrap:? (and #\| (esrap:? default-expr)))
         #\})
  (:destructure (kind open content default close)
    (declare (ignore open close))
    (let ((default (when default
                     (list :default (unless (equal (second default) "[]")
                                      (second default))))))
      (list* (ecase kind
               (:scalar :ref)
               (:list   :ref/list))
             content default))))

;;; Function call

#+no (esrap:defrule argument-expr/escaped-syntactic-character
    (and #\\ (or #\Space #\)))
  (:function second))

#+no (esrap:defrule argument-expr/content
    (+ (or argument-expr/escaped-syntactic-character
           (and (esrap:! (or whitespace #\) #\|)) text-character)))
  (:text t))

(esrap:defrule argument/yaml/block
    (and (* #\Space) #\Newline yaml/block)
  (:function third))

(esrap:defrule argument/flow
    (and whitespace* (or variable-reference
                         function-call
                         yaml/flow))
  (:function second))

(esrap:defrule argument
    (or argument/yaml/block
        argument/flow))

(esrap:defrule function-call
    (and evaluated-start #\( (+ argument) (and whitespace* #\)))
  (:destructure (kind open arguments close)
    (declare (ignore open close))
    (list* (ecase kind
             (:scalar :call)
             (:list   :call/list))
           arguments)))

(esrap:defrule expr
    (* (or variable-reference function-call text)))
