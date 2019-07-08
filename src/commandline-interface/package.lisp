;;;; package.lisp --- Package definition for the commandline-interface module.
;;;;
;;;; Copyright (C) 2013-2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:build-generator.commandline-interface
  (:use
   #:cl
   #:alexandria
   #:let-plus
   #:more-conditions)

  (:local-nicknames
   (#:options     #:configuration.options)

   (#:util        #:build-generator.util)

   (#:analysis    #:build-generator.analysis)

   (#:model       #:build-generator.model)
   (#:var         #:build-generator.model.variables)
   (#:project     #:build-generator.model.project)

   (#:commands    #:build-generator.commands)
   (#:commandline #:build-generator.commandline-options))

  (:export
   #:main)

  (:documentation
   "The commandline interface of the build-generator system."))
