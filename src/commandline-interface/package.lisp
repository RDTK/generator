;;;; package.lisp --- Package definition for the commandline-interface module.
;;;;
;;;; Copyright (C) 2013, 2014, 2015, 2016, 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:jenkins.project.commandline-interface
  (:use
   #:cl
   #:alexandria
   #:let-plus
   #:iterate
   #:more-conditions

   #:jenkins.model
   #:jenkins.model.variables
   #:jenkins.model.project)

  #+sbcl (:local-nicknames
          (#:clon #:net.didierverna.clon))

  (:shadowing-import-from #:jenkins.model.variables
   #:as)

  (:export
   #:main)

  (:documentation
   "The commandline interface of the jenkins.project system."))

#-sbcl (net.didierverna.clon:nickname-package)
