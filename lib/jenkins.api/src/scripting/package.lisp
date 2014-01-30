;;;; package.lisp --- Package definition for the scripting module.
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:jenkins.scripting
  (:use
   #:cl
   #:alexandria
   #:split-sequence
   #:let-plus
   #:iterate

   #:jenkins.api)

  (:export
   #:diff-configs)

  (:export
   #:assign-unique-ports)

  (:documentation
   "TODO"))
