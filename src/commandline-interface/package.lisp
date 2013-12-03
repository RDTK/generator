;;;; package.lisp --- Package definition for the commandline-interface module.
;;;;
;;;; Copyright (C) 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:jenkins.project.commandline-interface
  (:use
   #:cl
   #:let-plus)

  #+sbcl (:local-nicknames
          (#:clon #:com.dvlsoft.clon))

  (:export
   #:main)

  (:documentation
   "TODO"))

#-sbcl (com.dvlsoft.clon:nickname-package)
