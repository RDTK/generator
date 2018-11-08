;;;; files.lisp --- String-related utilities.
;;;;
;;;; Copyright (C) 2015, 2016, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.util)

(defun default-temporary-directory (&key
                                    (base #P"/tmp/")
                                    (hint "build-generator"))
  (assert (not (find #\/ hint)))

  (ensure-directories-exist base)
  (let* ((hint-pathname (make-pathname :name hint
                                       :type "XXXXXX"))
         (template      (namestring
                         (if hint
                             (merge-pathnames hint-pathname
                                              (parse-namestring base))
                             (parse-namestring base)))))
    (sb-ext:parse-native-namestring (sb-posix:mkdtemp template)
                                    nil *default-pathname-defaults*
                                    :as-directory t)))

(defun find-files (pattern &key (exclude "(\.svn|\.git)"))
  (let ((candidates (directory pattern)))
    (if exclude
        (remove-if (curry #'ppcre:scan exclude) candidates
                   :key #'namestring)
        candidates)))

(defun make-file-generator (directory patterns)
  (let+ ((patterns patterns)
         (files    nil)
         ((&labels next ()
            (cond
              (files
               (pop files))
              (patterns
               (let ((pattern (merge-pathnames (pop patterns) directory)))
                 (setf files (directory pattern))
                 (next)))))))
    #'next))

(defun safe-external-format-argument ()
  #+sbcl '(:external-format (:utf-8 :replacement #\?)))

(defun read-file-into-string* (filename &rest args &key external-format)
  (apply #'read-file-into-string filename
         (append (or external-format (safe-external-format-argument))
                 (remove-from-plist args :external-format))))
