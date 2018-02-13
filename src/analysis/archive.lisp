;;;; archive.lisp --- Access project that are distributed as archives.
;;;;
;;;; Copyright (C) 2014, 2015, 2017, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.analysis)

(defun download-file (url output-file &key username password)
  (let+ (((&values input code &ign &ign &ign close-stream? reason)
          (apply #'drakma:http-request url
                 :want-stream  t
                 :force-binary t
                 :verify       nil
                 (when (and username password)
                   (list :basic-authorization (list username password))))))
    (unless (<= 200 code 299)
      (error "~@<Download from ~A failed with code ~D: ~A.~@:>"
             url code reason))
    (unwind-protect
         (with-output-to-file (output output-file :element-type '(unsigned-byte 8))
           (copy-stream input output))
      (when close-stream?
        (close input)))))

(defun call-with-extracted-archive (thunk source temp-directory
                                    &key username password sub-directory)
  (let* ((archive-name (lastcar (puri:uri-parsed-path source)))
         (temp-file    (merge-pathnames archive-name temp-directory)))
    ;; Download into temporary archive file inside temporary
    ;; directory.
    (with-trivial-progress (:download "~A" source)
      (download-file source temp-file
                     :username username
                     :password password))
    ;; Extract temporary file, producing a single directory if
    ;; all goes well. Delete temporary archive file afterwards.
    (with-trivial-progress (:extract "~A" temp-file)
      (inferior-shell:run/nil `("unp" "-U" ,temp-file)
                              :directory temp-directory)
      (delete-file temp-file))
    ;; Locate the expected singleton directory and run analysis
    ;; on it.
    (let* ((directory (or (first (directory (merge-pathnames
                                             "*.*" temp-directory)))
                          (error "~@<Cannot locate directory
                                       extracted from ~A in ~A.~@:>"
                                 archive-name temp-directory)))
           (directory     (if sub-directory
                              (merge-pathnames sub-directory directory)
                              directory)))
      (funcall thunk directory))))

(defmacro with-extracted-archive ((directory (source temp-directory
                                              &key
                                              username
                                              password
                                              sub-directory))
                                  &body body)
  `(call-with-extracted-archive
    (lambda (,directory)
      ,@body)
    ,source ,temp-directory
    :username ,username :password ,password :sub-directory ,sub-directory))

(defmethod analyze ((source puri:uri) (kind (eql :archive))
                    &rest args &key
                    username
                    password
                    (versions       (missing-required-argument :versions))
                    sub-directory
                    (temp-directory (default-temporary-directory)))
  (with-extracted-archive (directory (source temp-directory
                                      :username      username
                                      :password      password
                                      :sub-directory sub-directory))
    (with-sequence-progress (:analyze/version versions)
      (mapcan
       (progressing
        (lambda (version)
          (with-simple-restart (continue "~<Ignore ~A and continue ~
                                          with the next version.~@:>"
                                         version)
            (let* ((other-args    (remove-from-plist
                                   args :username :password :versions
                                   :sub-directory :temp-directory))
                   (version-args  version)
                   (results       (apply #'analyze directory :auto
                                         (append version-args other-args))))
              (list (list* :scm              :archive
                           :branch-directory nil
                           results)))))
        :analyze/version)
       versions))))
