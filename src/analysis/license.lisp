;;;; license.lisp --- Analysis of license files.
;;;;
;;;; Copyright (C) 2013-2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.analysis)

(declaim (inline whitespace?))
(defun whitespace? (character)
  (or (char= character #\Space)
      (char= character #\Tab)
      (char= character #\Newline)
      (char= character #\Return)))

(defun normalize-text (string)
  (loop :for previous = nil :then current
        :for current :across string
        :for whitespace? = (whitespace? current)
        :when (and (not whitespace?) previous (whitespace? previous))
        :collect #\Space :into result
        :unless whitespace?
        :collect (char-downcase current) :into result
        :finally (return
                   (when result
                     (when (whitespace? (first result))
                       (pop result))
                     (coerce result 'simple-string)))))

(defun directory-licenses (directory)
  (loop :for file :in (directory (merge-pathnames "**/*.*" directory))
        :for name = (namestring (make-pathname :directory nil :defaults file))
        :collect (cons name (normalize-text
                             (read-file-into-string* file)))))

(defvar *licenses*
  (let ((system-licenses (directory-licenses "/usr/share/common-licenses/"))
        (extra-licenses  (directory-licenses (asdf:system-relative-pathname
                                              :jenkins.project "data/licenses/"))))
    (log:info "~@<~:[~
                 Not including any system licenses~
               ~;~:*~
                 Including system licenses~@:_~
                 ~<~{• ~A~^~@:_~}~:>~@:_~
               ~]~
               ~:[~
                 Not including any extra licenses~
               ~;~:*~
                 Including extra licenses~@:_~
                 ~<~{• ~A~^~@:_~}~:>~@:_~
               ~]~:>"
              (when system-licenses
                (list (map 'list #'first system-licenses)))
              (when extra-licenses
                (list (map 'list #'first extra-licenses))))
    (append system-licenses extra-licenses)))

(defun identify-license (text &key (known-licenses *licenses*) (threshold 100))
  (when-let ((normalized (normalize-text text))) ; Could even warn here if empty
    (let ((threshold (min (truncate threshold (/ (length normalized))) 2000)))
      (or ;; Fast path: exact match.
          (car (find normalized known-licenses :test #'string= :key #'cdr))
          ;; Slow path: edit distance.
          (car (find normalized known-licenses
                     :test (lambda (text license)
                             (< (edit-distance text license
                                               :upper-bound threshold)
                                threshold))
                     :key  #'cdr))))))

(defvar *license-file-patterns*
  '("COPYING.*" "LICENSE.*" "*/**/COPYING.*" "*/**/LICENSE.*"))

(defmethod analyze ((directory pathname)
                    (kind      (eql :license))
                    &key
                    (threshold .2))
  (with-trivial-progress (:analyze/license "~A" directory)
    (loop :with files = (make-file-generator
                         directory *license-file-patterns*)
          :for file = (funcall files)
          :while file
          :do (when-let* ((text    (read-file-into-string* file))
                          (license (identify-license text :threshold threshold)))
                (return-from analyze `(:license ,license))))))
