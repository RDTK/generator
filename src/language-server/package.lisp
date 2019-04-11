;;;; package.lisp --- Package definition for the language-server module.
;;;;
;;;; Copyright (C) 2016, 2017, 2018, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:jenkins.language-server
  (:use
   #:cl
   #:alexandria
   #:split-sequence
   #:let-plus)

  (:shadow
   #:variable)

  (:local-nicknames
   (#:sloc    #:text.source-location)
   (#:lookup  #:text.source-location.lookup)

   (#:lsp     #:protocol.language-server)
   (#:methods #:protocol.language-server.methods)
   (#:proto   #:protocol.language-server.protocol)
   (#:contrib #:protocol.language-server.contributor)

   (#:model   #:jenkins.model)
   (#:var     #:jenkins.model.variables)
   (#:project #:jenkins.model.project))

  (:export
   #:language-server))
