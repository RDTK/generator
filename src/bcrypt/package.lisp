;;;; package.lisp --- Package definition for the bcrypt module.
;;;;
;;;; Copyright (C) 2014-2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:build-generator.bcrypt
  (:use
   #:cl
   #:alexandria
   #:let-plus

   #:nibbles)

  ;; bcrypt-specific base64 encoding
  (:export
   #:base64-encode
   #:base64-decode)

  ;; Text format conversion
  (:export
   #:parse-password-hash
   #:print-password-hash)

  ;; Interface
  (:export
   #:make-salt
   #:hash-password)

  (:documentation
   "An implementation of the bcrypt key derivation."))
