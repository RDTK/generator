;;;; types.lisp --- Types used in the model.variables module.
;;;;
;;;; Copyright (C) 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.model.variables)

(deftype list-of (&whole whole thing)
  (let+ ((name/string (concatenate 'string "EVERY-" (symbol-name thing)))
         ((&flet name (package)
            (find-symbol name/string package)))
         (name (or (name (symbol-package thing))
                   (name (symbol-package 'list-of))
                   (error "~@<Could not derive predicate from ~S in ~
                           ~S.~@:>" thing whole))))
    `(and list (satisfies ,name))))

(defun every-string (thing)
  (and (listp thing) (every #'stringp thing)))
