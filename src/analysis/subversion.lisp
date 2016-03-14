;;;; subversion.lisp --- Analyze subversion repositories.
;;;;
;;;; Copyright (C) 2012, 2013, 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.analysis)

(defun %svn-and-global-options (&optional username password)
  `("svn" "--non-interactive" "--quiet"
          ,@(when username `("--username" ,username))
          ,@(when password `("--password" ,password))))

(defun %run-svn (spec directory &optional username password)
  (run `(,@(%svn-and-global-options username password) ,@spec) directory))

(defmethod analyze ((source puri:uri) (schema (eql :svn))
                    &rest args &key
                    username
                    password
                    (versions       (missing-required-argument :versions))
                    sub-directory
                    (temp-directory (default-temporary-directory)))
  (when sub-directory
    (assert (eq :relative (first (pathname-directory sub-directory)))))

  (let+ ((source (let ((source (puri:copy-uri source))) ; TODO make a function
                   (when-let ((path (puri:uri-path source)))
                     (unless (ends-with #\/ path)
                       (setf (puri:uri-path source)
                             (concatenate 'string path "/"))))
                   source))
         ((&flet analyze-directory (directory)
            (apply #'analyze directory :auto
                   (remove-from-plist args :username :password :versions
                                           :sub-directory :temp-directory))))
         ((&flet+ list-directories ((&whole version
                                     &key branch tag directory commit
                                     &allow-other-keys))
            (list version
                  (cond
                    ((and branch (string= branch "trunk"))
                     "trunk/")
                    (branch
                     (format nil "branches/~A/" branch))
                    (tag
                     (format nil "tags/~A/" tag))
                    (directory
                     (format nil "~A/" directory))
                    (t
                     ""))
                  commit)))
         (locations (mapcar #'list-directories versions))
         ((&flet analyze-location (name directory commit)
            (let ((repository-url  (reduce #'puri:merge-uris
                                           (append
                                            (when sub-directory
                                              (list (puri:uri (let ((s (namestring sub-directory)))
                                                                (if (ends-with #\/ s)
                                                                    (subseq s 0 (1- (length s)))
                                                                    s)))))
                                            (list (puri:uri directory) source))
                                           :from-end t))
                  (clone-directory (reduce #'merge-pathnames
                                           (append
                                            (when sub-directory
                                              (list sub-directory))
                                            (list (parse-namestring directory)
                                                  temp-directory)))))
              (log:info "~@<Checking out ~S -> ~S~@:>" repository-url clone-directory)
              (unwind-protect
                   (progn
                     (with-trivial-progress (:checkout "~A" source)
                       (%run-svn `("co" ,@(when commit `("-r" ,commit))
                                        ,(princ-to-string repository-url)
                                        ,clone-directory)
                                 temp-directory username password))

                     (let* ((result (list* :scm              :svn
                                           :branch-directory directory
                                           (append
                                            (list :commit commit)
                                            (analyze-directory clone-directory)))))
                       (unless (getf result :authors)
                         (setf (getf result :authors)
                               (analyze clone-directory :svn/authors
                                        :username username
                                        :password password)))
                       (cons name result)))

                (when (probe-file clone-directory)
                  (run `("rm" "-rf" ,clone-directory) temp-directory)))))))

    (with-sequence-progress (:analyze/branch locations)
      (iter (for (version directory commit) in locations)
            (progress "~A" version)
            (restart-case
                (collect (analyze-location version directory commit))
              (continue (&optional condition)
                :report (lambda (stream)
                          (format stream "~<Ignore ~A and continue ~
                                          with the next branch.~@:>"
                                  version))
                (declare (ignore condition))))))))

(defmethod analyze ((directory pathname) (kind (eql :svn/authors))
                    &key
                    username
                    password
                    (max-revisions 20)
                    (max-authors   5))
  (with-trivial-progress (:analyze/log "~A" directory)
    (let* ((lines
             (apply #'inferior-shell:run/nil
                    `(,@(%svn-and-global-options username password)
                      "log" "--limit" ,max-revisions ,directory)
                    :directory directory
                    :output    :lines
                    (safe-external-format-argument)))
           (frequencies (make-hash-table :test #'equal)))
      (dolist (line lines)
        (ppcre:register-groups-bind (author) ("r[0-9]+ \\| ([^|]+) \\|" line)
          (incf (gethash author frequencies 0))))
      (setf frequencies (sort (hash-table-alist frequencies) #'> :key #'cdr))
      (mapcar #'car (subseq frequencies 0 (min (length frequencies) max-authors))))))
