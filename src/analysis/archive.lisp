;;;; archive.lisp --- Access project that are distributed as archives.
;;;;
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.analysis)

(defmethod analyze ((source puri:uri) (kind (eql :archive))
                    &rest args &key
                    username
                    password
                    (branches       (missing-required-argument :branches))
                    tags ; TODO
                    sub-directory
                    (temp-directory (default-temporary-directory)))
  (with-sequence-progress (:analyze/branch branches)
    (iter (for branch       in   branches)
          (for archive-name next (lastcar (puri:uri-parsed-path source)))
          (for temp-file    next (merge-pathnames
                                  archive-name temp-directory))
          (progress "~A" branch)

          (restart-case
              (progn
                (with-trivial-progress (:download "~A" source)
                  (let+ (((&values input code &ign &ign &ign close-stream? reason)
                          (apply #'drakma:http-request source
                                 :want-stream  t
                                 :force-binary t
                                 (when (and username password)
                                   (list :basic-authorization (list username password))))))
                    (unless (<= 200 code 299)
                      (error "~@<Download from ~A failed with code ~D: ~A.~@:>"
                             source code reason))
                    (unwind-protect
                         (with-output-to-file (output temp-file :element-type '(unsigned-byte 8))
                           (copy-stream input output))
                      (when close-stream?
                        (close input)))))

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
                                                        (remove-from-plist args :username :password :branches :tags
                                                                                :sub-directory :temp-directory)))))
                  (collect (cons branch result))))
            (continue (&optional condition)
              :report (lambda (stream)
                        (format stream "~<Ignore ~A and continue ~
                                        with the next branch.~@:>"
                                branch))
              (declare (ignore condition)))))))
