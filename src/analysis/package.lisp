;;;; package.lisp --- Package definition for the analysis module.
;;;;
;;;; Copyright (C) 2012-2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:jenkins.analysis
  (:use
   #:cl
   #:alexandria
   #:split-sequence
   #:iterate
   #:let-plus
   #:more-conditions)

  (:local-nicknames
   (#:util    #:jenkins.util)
   (#:version #:jenkins.version))

  ;; Conditions
  (:export
   #:analysis-condition
   #:analysis-condition-specification

   #:analysis-error

   #:repository-access-error

   #:repository-analysis-error

   #:project-analysis-error

   #:dependency-condition
   #:dependency-condition-dependency

   #:unfulfilled-project-dependency-error
   #:unfulfilled-project-dependency-candidates

   #:unfulfilled-platform-dependency-error)

  ;; Natures, targets and dependencies
  (:export
   #:same-target?
   #:target-matches?

   #:dependency-matches?

   #:merge-dependencies
   #:effective-requires)

  ;; Analysis protocol
  (:export
   #:analyze)

  (:export
   #:current-platform
   #:installed-packages)

  (:documentation
   "Functions for analyzing various kinds of projects."))
