;;;; subversion.lisp --- Analyze subversion repositories.
;;;;
;;;; Copyright (C) 2012-2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.analysis)

(defun %svn-and-global-options (&optional username password)
  `("svn" "--non-interactive" "--quiet"
          ,@(when username `("--username" ,username))
          ,@(when password `("--password" ,password))))

(defun %run-svn (spec directory &optional username password)
  (run `(,@(%svn-and-global-options username password) ,@spec) directory))

(defun subversion-checkout (repository directory commit temp-directory
                            &key username password)
  (with-condition-translation (((error repository-access-error)
                                :specification repository))
    (%run-svn `("co" ,@(when commit `("-r" ,commit))
                     ,(princ-to-string repository)
                     ,directory)
              temp-directory username password)))

(defun checkout-subversion-repository (source clone-directory
                                       &rest args
                                       &key
                                       username
                                       password
                                       commit
                                       sub-directory
                                       temp-directory
                                       &allow-other-keys)
  (log:info "~@<Checking out ~S -> ~S~@:>" source target)
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
    (with-trivial-progress (:checkout "~A" source)
      (subversion-checkout repository-url clone-directory commit temp-directory
                           :username username :password password))))

(defmethod analyze ((source puri:uri) (schema (eql :svn))
                    &rest args &key
                    username
                    password
                    (versions       (missing-required-argument :versions))
                    sub-directory
                    (temp-directory (default-temporary-directory)))
  (when sub-directory
    (assert (eq :relative (first (pathname-directory sub-directory)))))

  (let+ ((source (ensure-directory-uri source))
         ((&flet analyze-directory (version directory)
            (apply #'analyze directory :auto
                   (append
                    (remove-from-plist
                     version :branch :tag :commit :directory)
                    (remove-from-plist
                     args :username :password :versions
                          :sub-directory :temp-directory)))))
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
         ((&flet analyze-location (version directory commit)
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
                       (subversion-checkout
                        repository-url clone-directory commit temp-directory
                        :username username :password password))
                     (let ((committers (analyze clone-directory :svn/committers
                                                :username username
                                                :password password))
                           (result     (analyze-directory version clone-directory)))
                       (list* :scm              :svn
                              :branch-directory directory
                              :commit           commit
                              :committers       committers
                              result)))

                (when (probe-file clone-directory)
                  (run `("rm" "-rf" ,clone-directory) temp-directory)))))))

    (with-sequence-progress (:analyze/branch locations)
      (iter (for (version directory commit) in locations)
            (progress "~A" version)
            (with-simple-restart
                (continue "~<Ignore ~A and continue with the next ~
                           branch.~@:>"
                          version)
              (collect (analyze-location version directory commit)))))))

(defmethod analyze ((directory pathname) (kind (eql :svn/committers))
                    &key
                    username
                    password
                    (max-revisions  20)
                    (max-committers 5))
  (with-trivial-progress (:analyze/log "~A" directory)
    (let* ((lines
             (apply #'inferior-shell:run/nil
                    `(,@(%svn-and-global-options username password)
                      "log" "--limit" ,max-revisions ,directory)
                    :directory directory
                    :output    :lines
                    (safe-external-format-argument)))
           (person-collector (make-names->person-list :count max-committers)))
      (dolist (line lines)
        (ppcre:register-groups-bind (author) ("r[0-9]+ \\| ([^|]+) \\|" line)
          (funcall person-collector author)))
      (funcall person-collector))))
