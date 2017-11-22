;;;; context.lisp --- TODO.
;;;;
;;;; Copyright (C) 2016, 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.language-server)

(defclass context ()
  ((connection :initarg  :connection
               :reader   connection)
   (workspace  :reader   workspace
               :writer   (setf %workspace)
               :initform nil)))
