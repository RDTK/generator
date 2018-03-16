;;;; package.lisp --- Package definition for the commandline-options module.
;;;;
;;;; Copyright (C) 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:jenkins.project.commandline-options
  (:use
   #:cl
   #:alexandria
   #:let-plus
   #:more-conditions)

  ;; Conditions
  (:export
   #:option-context-condition
   #:context

   #:context-not-found-error

   #:option-condition
   #:option

   #:option-not-found-error)

  ;; Option info protocol
  (:export
   #:option-value)

  ;; Option contexts
  (:export
   #:find-options
   #:register-option)

  ;; Individual options
  (:export
   #:find-option
   #:value-for-option)

  ;; High-level interface
  (:export
   #:map-commandline-options)

  ;; Macros
  (:export
   #:define-option-mapping)

  (:documentation
   "Handling of commandline options.

    Based on a configuration option schema, this package allows
    associating commandline option information to configuration
    options and to process commandline argument according to this
    information.

    The option information is organized into several contexts
    corresponding to global commandline options and
    sub-command-specific commandline options."))
