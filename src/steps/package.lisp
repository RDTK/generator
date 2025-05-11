;;;; package.lisp --- Package definition for the steps module.
;;;;
;;;; Copyright (C) 2014-2025 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:build-generator.steps
  (:use
   #:cl
   #:alexandria
   #:let-plus
   #:more-conditions)

  (:local-nicknames
   (#:analysis #:build-generator.analysis)

   (#:res      #:build-generator.resources))

  ;; Step protocol
  (:export
   #:execute)

  ;; Step creation protocol
  (:export
   #:make-step)

  ;; Step runtime support
  (:export
   #:map-with-restart/sequential #:map-with-restart/sequential/progress
   #:map-with-restart/parallel   #:map-with-restart/parallel/progress)

  ;; Step macros
  (:export
   #:with-sequence-processing

   #:define-step
   #:define-sequence-step)

  ;; Jenkins install steps
  (:export
   #:jenkins-username?
   #:jenkins-username))
