;;;; functions-version.lisp --- Functions related to the program version.
;;;;
;;;; Copyright (C) 2017, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.project.commands)

;;; Version

(defparameter *generator-version*
  (asdf:component-version (asdf:find-system :jenkins.project)))

(defun generator-version ()
  *generator-version*)

;;; Changelog

(deftype changelog-level ()
  '(member nil :enhancement :bugfix :incompatible-change t))

(defparameter *changelog*
  (uiop:read-file-form (asdf:system-relative-pathname
                        :jenkins.project.commands "changes.sexp")))

(defun changelog (&key count)
  (let ((changelog *changelog*))
    (subseq changelog 0 (min (length changelog)
                             (or count most-positive-fixnum)))))

(defun print-changelog (changelog &key (stream *standard-output*))
  (labels ((rec (stream item)
             (optima:ematch item

               ((list* :release version changes)
                (format stream "Release ~A~:@_" version)
                (if changes
                    (map nil (lambda (change)
                               (rec stream change)
                               (format stream "~@:_~@:_"))
                         changes)
                    (format stream "«no changes yet»~@:_")))

               ((list* (and kind (type changelog-level)) body)
                (format stream "• ~A~:@_  " kind)
                (pprint-logical-block (stream body :per-line-prefix "")
                  (rec stream body)))

               ((list* :ul items)
                (pprint-newline :linear stream)
                (mapl (lambda+ ((first &rest rest))
                        (format stream "• ")
                        (pprint-logical-block (stream (ensure-list first)
                                                      :per-line-prefix "")
                          (rec stream first))
                        (when rest
                          (pprint-newline :mandatory stream)))
                      items))

               ((list :verb (and content (type string)))
                (write-string content stream))

               ((list* (type symbol) body)
                (map nil (curry #'rec stream) body))

               ((type cons)
                (mapl (lambda+ ((item &rest rest))
                        (rec stream item)
                        (when rest
                          (format stream " ~:_")))
                      item))

               ((type string)
                (format stream "~{~A~^ ~:_~}"
                        (split-sequence:split-sequence-if
                         (lambda (element)
                           (member element '(#\Space #\Newline)))
                         item :remove-empty-subseqs t))))))
    (pprint-logical-block (stream changelog)
      (map nil (lambda (release)
                 (rec stream release)
                 (pprint-newline :mandatory stream))
           changelog))))

