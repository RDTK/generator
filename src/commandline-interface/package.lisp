;;;; package.lisp --- Package definition for the commandline-interface module.
;;;;
;;;; Copyright (C) 2013-2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:jenkins.project.commandline-interface
  (:use
   #:cl
   #:alexandria
   #:let-plus
   #:more-conditions

   #:jenkins.model
   #:jenkins.model.variables
   #:jenkins.model.project)

  (:shadowing-import-from #:jenkins.model.variables
   #:as)

  (:export
   #:main)

  (:documentation
   "The commandline interface of the jenkins.project system."))
