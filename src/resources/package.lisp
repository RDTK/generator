;;;; package.lisp --- Package definition for the resources module.
;;;;
;;;; Copyright (C) 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:build-generator.resources
  (:use
   #:cl
   #:alexandria
   #:more-conditions)

  ;; Conditions
  (:export
   #:entry-does-not-exist-error
   #:name
   #:group

   #:group-does-not-exist-error
   #:name
   #:resources)

  ;; Name protocol
  (:export
   #:name)

  ;; Size protocol
  (:export
   #:octet-count)

  ;; Entry protocol
  (:export
   #:content
   #:info)

  ;; Group protocol
  (:export
   #:parent

   #:entries
   #:find-entry                     ; also `setf'
   #:add-file)

  ;; Resources protocol
  (:export
   #:find-group                     ; also `setf'

   #:ensure-datum)

  ;; Global resource group registry
  (:export
   #:make-group
   #:find-group*))
