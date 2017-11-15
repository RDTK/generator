;;;; license.lisp --- Analysis of license files.
;;;;
;;;; Copyright (C) 2013, 2014, 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.analysis)

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
                 (let ((content  (read-file-into-string* project-file))
                       (licenses (directory "/usr/share/common-licenses/**/*.*")))
                   (or
                    ;; Fast path: exact match.
                    (find content licenses
                          :test #'string=
                          :key  #'read-file-into-string*)
                    ;; Slow path: edit distance.
                    (find content licenses
                          :test (lambda (x y) (< (edit-distance x y :upper-bound threshold) threshold))
                          :key #'read-file-into-string*)))))
      `(:license ,(pathname-name system-file)))))
