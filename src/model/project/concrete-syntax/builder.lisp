;;;; builder.lisp --- Builder used for recipe concrete syntax.
;;;;
;;;; Copyright (C) 2016-2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.model.project)

(defclass recipe-builder (language.yaml.construct::native-builder)
  ())

(defun make-builder (source)
  (make-instance 'text.source-location.source-tracking-builder::callback-source-tracking-builder
                 :target   (make-instance 'recipe-builder)
                 :source   source
                 :callback (lambda (object location)
                             (setf (location-of object) location))))
