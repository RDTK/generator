;;;; package.lisp --- Package definition for the model.variables module.
;;;;
;;;; Copyright (C) 2012-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:jenkins.model.variables
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

   #:undefined-variable-condition
   #:undefined-variable-warning
   #:undefined-variable-error)

  ;; Variables
  (:export
   #:*traced-variables*)

  ;; Value protocol
  (:export
   #:value-list
   #:value-list*
   #:value-cons
   #:value-acons
   #:value-parse)

  ;; Lookup protocol
  (:export
   #:lookup
   #:expand
   #:value

   #:as)

  ;; Variable protocol and mixin class
  (:export
   #:direct-variables
   #:variables

   #:direct-variables-mixin)

  ;; Variable schema protocol
  (:export
   #:variable-info-name
   #:variable-info-type
   #:variable-info-documentation

   #:all-variables
   #:find-variable ; also setf
   #:note-variable

   #:define-variable

   #:check-variable-access)

  (:documentation
   "Variable model, syntax, lookup, expansion and schema."))
