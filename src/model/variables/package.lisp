;;;; package.lisp --- Package definition for the model.variables module.
;;;;
;;;; Copyright (C) 2012-2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:build-generator.model.variables
  (:use
   #:cl
   #:alexandria
   #:let-plus
   #:more-conditions)

  ;; Conditions
  (:export
   #:expression-cycle-error
   #:expression-cycle-error-path

   #:variable-condition
   #:variable-condition-name

   #:unused-variable-warning

   #:undefined-variable-condition
   #:undefined-variable-warning
   #:undefined-variable-error)

  ;; Types
  (:export
   #:list-of)

  ;; Variables
  (:export
   #:*traced-variables*)

  ;; Value protocol
  (:export
   #:value-list
   #:value-list*
   #:value-cons
   #:value-acons
   #:value-parse
   #:value-unparse

   #:to-value)

  ;; Lookup protocol
  (:export
   #:merge-lookup-results
   #:merge-lookup-values

   #:direct-lookup
   #:lookup                         ; also setf
   #:expand
   #:value
   #:evaluate

   #:as
   #:value/cast

   #:aggregate-values)

  ;; Variable protocol and mixin class
  (:export
   #:direct-variables
   #:variables

   #:direct-variables-mixin)

  ;; Builtin entries protocol and mixin class
  (:export
   #:builtin-entries

   #:builtin-entries-mixin)

  ;; Variable schema protocol
  (:export
   #:variable-info
   #:variable-info-name
   #:variable-info-type
   #:inheritance
   #:aggregation
   #:variable-info-documentation

   #:make-variable-info

   #:all-variables
   #:find-variable ; also setf
   #:note-variable

   #:define-variable

   #:check-variable-liveness
   #:check-variable-access)

  (:documentation
   "Variable model, syntax, lookup, expansion and schema."))
