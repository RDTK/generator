;;;; files.lisp --- String-related utilities.
;;;;
;;;; Copyright (C) 2015, 2016, 2018, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:build-generator.util)

;;; Namestrings

(defun safe-enough-namestring (pathname &optional defaults)
  (let* ((safe-directory (substitute :up :back (pathname-directory pathname)))
         (safe-pathname  (make-pathname :directory safe-directory
                                        :defaults  pathname)))
    (if defaults
        (enough-namestring safe-pathname defaults)
        (enough-namestring safe-pathname))))

;;; Temporary directories

(defclass temporary-directory ()
  ((%base      :initarg  :base
               :type     pathname
               :reader   base)
   (%hint      :initarg :hint
               :type string
               :reader hint)
   (%directory :accessor directory
               :type     (or null pathname)
               :initform nil)))

(defmethod ensure-exists ((temporary-directory temporary-directory))
  (or (directory temporary-directory)
      (let+ (((&accessors base hint directory) temporary-directory)
             (hint-pathname (make-pathname :name hint :type "XXXXXX"))
             (template      (if hint
                                (merge-pathnames hint-pathname base)
                                base)))
        (ensure-directories-exist base)
        (setf directory (sb-ext:parse-native-namestring
                         (sb-posix:mkdtemp template)
                         nil *default-pathname-defaults*
                         :as-directory t)))))

(defmethod ensure-deleted ((temporary-directory temporary-directory))
  (when-let ((directory (directory temporary-directory)))
    (uiop:delete-directory-tree
     directory :validate t :if-does-not-exist :ignore)))

(defun make-temporary-directory (&key
                                 (base #P"/tmp/")
                                 (hint "build-generator"))
  (assert (not (find #\/ hint)))
  (make-instance 'temporary-directory :base (pathname base) :hint hint))

(defclass temporary-sub-directory ()
  ((%parent    :initarg  :parent
               :reader   parent)
   (%name      :initarg  :name
               :reader   name)
   (%directory :accessor directory
               :initform nil)))

(defmethod ensure-exists ((temporary-directory temporary-sub-directory))
  (or (directory temporary-directory)
      (let+ (((&accessors parent name directory) temporary-directory)
             (base (ensure-exists parent)))
        (setf directory (ensure-directories-exist
                         (merge-pathnames name base))))))

(defun make-temporary-sub-directory (parent name)
  (make-instance 'temporary-sub-directory :parent parent :name name))

;;; Finding files

(defun find-files (pattern &key (exclude "(\.svn|\.git)"))
  (let ((candidates (cl:directory pattern)))
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
                 (setf files (cl:directory pattern))
                 (next)))))))
    #'next))

(defun safe-external-format-argument ()
  #+sbcl '(:external-format (:utf-8 :replacement #\?)))

(defun read-file-into-string* (filename &rest args &key external-format)
  (apply #'read-file-into-string filename
         (append (or external-format (safe-external-format-argument))
                 (remove-from-plist args :external-format))))
