;;;; license.lisp --- Analysis of license files.
;;;;
;;;; Copyright (C) 2013, 2014, 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.analysis)

(defun identify-license (text
                         &key
                         (known-licenses (directory "/usr/share/common-licenses/**/*.*"))
                         (threshold 200))
  (or ;; Fast path: exact match.
      (find text known-licenses :test #'string= :key #'read-file-into-string*)
      ;; Slow path: edit distance.
      (find text known-licenses
            :test (lambda (x y) (< (edit-distance x y :upper-bound threshold) threshold))
            :key #'read-file-into-string*)))

(defmethod analyze ((directory pathname)
                    (kind      (eql :license))
                    &key
                    (threshold 200))
  (with-trivial-progress (:analyze/license "~A" directory)
    (when-let* ((project-file (first
                               (append
                                (find-files
                                 (merge-pathnames "COPYING.*" directory))
                                (find-files
                                 (merge-pathnames "LICENSE.*" directory))
                                (find-files
                                 (merge-pathnames "**/COPYING.*" directory))
                                (find-files
                                 (merge-pathnames "**/LICENSE.*" directory)))))
                (system-file
                 (identify-license (read-file-into-string* project-file)
                                   :threshold threshold)))
      `(:license ,(pathname-name system-file)))))
