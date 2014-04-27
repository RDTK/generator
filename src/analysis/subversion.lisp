;;;; subversion.lisp --- Analyze subversion repositories.
;;;;
;;;; Copyright (C) 2012, 2013, 2014 Jan Moringen
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
                    &key
                    username
                    password
                    branches
                    tags
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
         ((&flet list-directories (directory spec)
            (mapcar (lambda (name)
                      (if (and (string= directory "branches")
                               (string= name "trunk"))
                          (list "trunk" "trunk/")
                          (list name (format nil "~A/~A/" directory name))))
                    spec)))
         (locations (append (when branches
                              (list-directories "branches" branches))
                            (when tags
                              (list-directories "tags" tags))))
         ((&flet analyze-branch (name directory)
            (let ((repository-url  (reduce #'puri:merge-uris
                                           (append
                                            (when sub-directory
                                              (list (puri:uri (let ((s (namestring sub-directory)))
                                                                (if (ends-with #\/ s)
                                                                    (subseq s 0 (1- (length s)))
                                                                    s)))))
                                            (list
                                             (puri:uri directory)
                                             source))
                                           :from-end t))
                  (clone-directory (reduce #'merge-pathnames
                                           (append
                                            (when sub-directory
                                              (list sub-directory))
                                            (list
                                             (parse-namestring directory)
                                             temp-directory)))))
              (log:info "~@<Checking out ~S -> ~S~@:>" repository-url clone-directory)
              (unwind-protect
                   (progn
                     (with-trivial-progress (:checkout "~A" source)
                       (%run-svn `("co" ,(princ-to-string repository-url) ,clone-directory)
                                 temp-directory username password))

                     (let* ((result (list* :scm              :svn
                                           :branch-directory directory
                                           (analyze clone-directory :auto))))
                       (unless (getf result :authors)
                         (setf (getf result :authors)
                               (analyze clone-directory :svn/authors
                                        :username username
                                        :password password)))
                       (cons name result)))

                (run `("rm" "-rf" ,clone-directory) temp-directory))))))

    (with-sequence-progress (:analyze/branch locations)
      (iter (for (name directory) in locations)
            (progress "~A" name)
            (restart-case
                (collect (analyze-branch name directory))
              (continue (&optional condition)
                :report (lambda (stream)
                          (format stream "~<Ignore ~A and continue ~
                                          with the next branch.~@:>"
                                  name))
                (declare (ignore condition))))))))

(defmethod analyze ((directory pathname) (kind (eql :svn/authors))
                    &key
                    username
                    password
                    (max-revisions 20)
                    (max-authors   5))
  (with-trivial-progress (:analyze/log "~A" directory)
    (let* ((lines
             (inferior-shell:run/lines
              `(,@(%svn-and-global-options username password)
                "log" "--limit" ,max-revisions ,directory)
              :directory directory))
           (frequencies (make-hash-table :test #'equal)))
      (dolist (line lines)
        (ppcre:register-groups-bind (author) ("r[0-9]+ \\| ([^|]+) \\|" line)
          (incf (gethash author frequencies 0))))
      (setf frequencies (sort (hash-table-alist frequencies) #'> :key #'cdr))
      (mapcar #'car (subseq frequencies 0 (min (length frequencies) max-authors))))))
