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
   #:more-conditions)

  (:local-nicknames
   (#:options     #:configuration.options)

   (#:util        #:jenkins.util)

   (#:analysis    #:jenkins.analysis)

   (#:model       #:jenkins.model)
   (#:var         #:jenkins.model.variables)
   (#:project     #:jenkins.model.project)

   (#:commands    #:jenkins.project.commands)
   (#:commandline #:jenkins.project.commandline-options))

  (:export
   #:main)

  (:documentation
   "The commandline interface of the jenkins.project system."))
