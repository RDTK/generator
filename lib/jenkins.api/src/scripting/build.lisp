;;;; build.lisp ---
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.scripting)

(defun rebuild-failed ()
  (dolist (build (last-builds))
    (when (eq (result build) :failure)
      (format t "Failed build ~A -> building ~A~%"
              build (job build))
      (handler-case
          (build! (job build))
        (error (condition)
          (format t "Failed to trigger build for job ~A: ~A~%"
                  (job build) condition))))))
