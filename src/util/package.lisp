;;;; package.lisp --- Package definition for the util module.
;;;;
;;;; Copyright (C) 2018, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:jenkins.util
  (:use
   #:cl
   #:alexandria
   #:let-plus
   #:iterate)

  ;; Conditions and Restarts
  (:export
   #:some-cause

   #:continuable-error
   #:find-continue-restart

   #:call-with-retry-restart
   #:with-retry-restart
   #:call-with-retries
   #:with-retries)

  ;; Strings
  (:export
   #:safe-name

   #:edit-distance
   #:closest-matches)

  ;; Files
  (:export
   #:safe-enough-namestring

   #:default-temporary-directory
   #:find-files
   #:make-file-generator
   #:safe-external-format-argument
   #:read-file-into-string*)

  ;; Sorting
  (:export
   #:sort-with-partial-order))
