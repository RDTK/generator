;;;; types.lisp --- Types used by the api module.
;;;;
;;;; Copyright (C) 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.api)

(deftype unmapped-marker ()
  "Objects of this type are used as placeholders for unmapped
interface implementations."
  `(cons (eql :unimplemented) (cons symbol (cons string (cons t null)))))
