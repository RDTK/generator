;;;; license.lisp --- Analysis of license files.
;;;;
;;;; Copyright (C) 2013, 2014, 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.analysis)

(defun directory-licenses (directory)
  (loop :for file :in (directory (merge-pathnames "**/*.*" directory))
     :for name = (namestring (make-pathname :directory nil :defaults file))
     :collect (cons name (read-file-into-string* file))))

(defvar *licenses*
  (let ((system-licenses (directory-licenses "/usr/share/common-licenses/")))
    (log:info "~@<~:[~
                 Not including any system licenses~
               ~;~:*~
                 Including system licenses~@:_~
                 ~<~{• ~A~^~@:_~}~:>~@:_~
               ~]~:>"
              (when system-licenses
                (list (map 'list #'first system-licenses))))
    system-licenses))

(defun identify-license (text
                         &key
                         (known-licenses *licenses*)
                         (threshold 200))
  (or ;; Fast path: exact match.
      (car (find text known-licenses :test #'string= :key #'cdr))
      ;; Slow path: edit distance.
      (car (find text known-licenses
                 :test (lambda (x y)
                         (< (edit-distance x y :upper-bound threshold) threshold))
                 :key #'cdr))))

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
                (license
                 (identify-license (read-file-into-string* project-file)
                                   :threshold threshold)))
      `(:license ,license))))
