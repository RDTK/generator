;;;; git.lisp ---
;;;;
;;;; Copyright (C) 2012, 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.analysis)

(defun %run-git (spec directory)
  (run `("git" ,@spec) directory))

(defmethod analyze ((source puri:uri) (schema (eql :git))
                    &rest args &key
                    username
                    password
                    branches
                    tags
                    sub-directory
                    history-limit
                    (temp-directory (default-temporary-directory)))
  "TODO(jmoringe): document"
  ;;; TODO(jmoringe, 2013-01-17): find branches automatically
  (let+ ((project-name      (lastcar (puri:uri-parsed-path source)))
         (repository/string (format-git-url source username password))
         (branches          (append branches tags))
         (clone-directory   (merge-pathnames
                             (make-pathname :directory (list :relative project-name))
                             temp-directory))
         (analyze-directory (if sub-directory
                                (merge-pathnames sub-directory clone-directory)
                                clone-directory))
         ((&flet analyze-directory (directory)
            (apply #'analyze directory :auto
                   (remove-from-plist args :username :password :branches :tags
                                           :sub-directory :history-limit
                                           :temp-directory)))))

    (unwind-protect
         (progn
           (with-trivial-progress (:clone "~A" repository/string)
             (%run-git
              `("clone" "--quiet"
                ,@(when history-limit `("--depth" ,history-limit))
                ,repository/string ,clone-directory)
              temp-directory))

           (with-sequence-progress (:analyze/branch branches)
             (iter (for branch in branches)
                   (progress "~A" branch)

                   (restart-case
                       (progn
                         (%run-git
                          `("--work-tree" ,clone-directory
                            "--git-dir" ,(merge-pathnames ".git/" clone-directory)
                            "checkout" "--quiet" ,branch)
                          clone-directory)

                         (let ((result (list* :scm              :git
                                              :branch-directory nil
                                              (analyze-directory analyze-directory))))
                           (unless (getf result :authors)
                             (setf (getf result :authors)
                                   (analyze clone-directory :git/authors)))
                           (collect (cons branch result))))
                     (continue (&optional condition)
                       :report (lambda (stream)
                                 (format stream "~<Ignore ~A and ~
                                               continue with the next ~
                                               branch.~@:>"
                                         branch))
                       (declare (ignore condition)))))))

      (run `("rm" "-rf" ,clone-directory) temp-directory))))

(defmethod analyze ((directory pathname) (kind (eql :git/authors))
                    &key
                    (max-authors 5))
  (with-trivial-progress (:analyze/log "~A" directory)
    (let* ((lines
             (apply #'inferior-shell:run/nil
                    `("git" "--no-pager" ,(format nil "--git-dir=~A/.git" directory)
                            "log" "--pretty=format:%an <%ae>")
                    :directory directory ; TODO is directory even needed?
                    :output    :lines
                    (safe-external-format-argument)))
           (frequencies (make-hash-table :test #'equal)))
      (dolist (line lines)
        (incf (gethash line frequencies 0)))
      (setf frequencies (sort (hash-table-alist frequencies) #'> :key #'cdr))
      (mapcar #'car (subseq frequencies 0 (min (length frequencies) max-authors))))))
