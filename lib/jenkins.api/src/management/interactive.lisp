;;;; interactive.lisp ---
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.management)

(defun visit (job &optional suffix)
  (sb-ext:run-program
   "firefox"
   (list "-new-tab" (format nil "~A/job/~A~@[/~A~]"
                            *base-url* (id job) suffix))
   :search t
   :wait   nil))
