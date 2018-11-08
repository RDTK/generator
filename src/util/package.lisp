;;;; package.lisp --- Package definition for the util module.
;;;;
;;;; Copyright (C) 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:jenkins.util
  (:use
   #:cl
   #:alexandria
   #:let-plus
   #:iterate)

  ;; Restarts
  (:export
   #:call-with-retry-restart
   #:with-retry-restart
   #:call-with-retries
   #:with-retries)

  ;; Strings
  (:export
   #:safe-name
   #:edit-distance)

  ;; Files
  (:export
   #:default-temporary-directory
   #:find-files
   #:make-file-generator
   #:safe-external-format-argument
   #:read-file-into-string*))
