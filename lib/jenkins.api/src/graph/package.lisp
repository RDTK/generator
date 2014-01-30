;;;; package.lisp --- Package definition for the graph module.
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:jenkins.graph
  (:use
   #:cl
   #:alexandria

   #:jenkins.management)

  (:export
   #:make-graph
   #:make-graph/file)

  (:documentation
   "TODO"))
