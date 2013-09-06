;;;; package.lisp --- Package definition for project module.
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:jenkins.project
  (:use
   #:cl
   #:alexandria
   #:split-sequence
   #:iterate
   #:let-plus
   #:more-conditions
   #:print-items

   #:jenkins.api
   #:jenkins.dsl)

  (:shadowing-import-from #:jenkins.dsl
   #:job)

  (:import-from #:jenkins.analysis
   #:version>=
   #:version-matches)

  ;; Conditions
  (:export
   #:instantiation-condition
   #:instantiation-condition-specification

   #:instantiation-error

   #:deployment-condition
   #:deployment-condition-thing

   #:deployment-error)

  ;; Template protocol
  (:export
   #:find-template
   )

  ;; Project specification protocol
  (:export
   #:templates
   #:versions
   #:jobs)

  ;; Version specification protocol
  (:export
   #:dependencies)

  ;; Job specification protocol
  (:export
   #:tags)

  ;; Template specification protocol
  (:export
   #:inherit
   #:aspects
   #:jobs)

  ;; Aspect specification protocol
  (:export
   #:aspects
   #:filter)

  ;; Project protocol
  (:export
   #:find-project)

  ;; Instance protocol
  (:export
   #:find-instance)

  (:documentation
   "TODO"))
