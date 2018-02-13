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

(defmethod analyze ((source puri:uri) (kind (eql :archive))
                    &rest args &key
                    username
                    password
                    (versions       (missing-required-argument :versions))
                    sub-directory
                    (temp-directory (default-temporary-directory)))
  (with-sequence-progress (:analyze/version versions)
    (iter (for version      in   versions)
          (for archive-name next (lastcar (puri:uri-parsed-path source)))
          (for temp-file    next (merge-pathnames
                                  archive-name temp-directory))
          (progress "~A" version)

          (with-simple-restart
              (continue "~<Ignore ~A and continue with the next ~
                         version.~@:>"
                        version)
            (with-trivial-progress (:download "~A" source)
              (download-file source temp-file
                             :username username
                             :password password))

            (with-trivial-progress (:extract "~A" temp-file)
              (inferior-shell:run/nil `("unp" "-U" ,temp-file) :directory temp-directory)
              (delete-file temp-file))

            (let* ((analyze-directory (merge-pathnames
                                       (or sub-directory
                                           (first (directory (merge-pathnames "*.*" temp-directory)))) ; TODO
                                       temp-directory))
                   (result            (list* :scm              :archive
                                             :branch-directory nil
                                             (apply #'analyze analyze-directory :auto
                                                    (append
                                                     version
                                                     (remove-from-plist args :username :password :versions
                                                                             :sub-directory :temp-directory))))))
              (collect result))))))
