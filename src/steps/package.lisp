;;;; package.lisp --- Package definition for the steps module.
;;;;
;;;; Copyright (C) 2014-2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:jenkins.project.steps
  (:use
   #:cl
   #:alexandria
   #:let-plus
   #:more-conditions)

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
   #:define-sequence-step))
