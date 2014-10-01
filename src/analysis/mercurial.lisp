;;;; mercurial.lisp --- Support for the mercurial DVCS.
;;;;
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.analysis)

(defun %run-mercurial (spec directory)
  (run `("hg" ,@spec) directory))

(defmethod analyze ((source puri:uri) (schema (eql :mercurial))
                    &rest args &key
                    username
                    password
                    branches
                    tags
                    sub-directory
                    (temp-directory (default-temporary-directory)))
  (when (or username password)
    (error "~@<Authentication support not implemented for ~A.~@:>" schema))

  (let+ ((repository/string (princ-to-string source))
         (branches          (append branches tags))
         (clone-directory   temp-directory)
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
             (%run-mercurial
              `("clone" ,repository/string ,clone-directory)
              temp-directory))

           (with-sequence-progress (:analyze/branch branches)
             (iter (for branch in branches)
                   (progress "~A" branch)

                   (restart-case
                       (progn
                         (%run-mercurial `("checkout" ,branch) clone-directory)

                         (let ((result (list* :scm              :mercurial
                                              :branch-directory nil
                                              (analyze-directory analyze-directory))))
                           (unless (getf result :authors)
                             (setf (getf result :authors)
                                   (analyze clone-directory :mercurial/authors)))
                           (collect (cons branch result))))
                     (continue (&optional condition)
                       :report (lambda (stream)
                                 (format stream "~<Ignore ~A and ~
                                                 continue with the ~
                                                 next branch.~@:>"
                                         branch))
                       (declare (ignore condition)))))))

      (run `("rm" "-rf" ,clone-directory) temp-directory))))

(defmethod analyze ((directory pathname) (kind (eql :mercurial/authors))
                    &key
                    (max-authors 5))
  (with-trivial-progress (:analyze/log "~A" directory)
    (let* ((lines
             (apply #'inferior-shell:run/nil
                    `("hg" "log" "--template" "{author}\\n")
                    :directory directory
                    :output    :lines
                    (safe-external-format-argument)))
           (frequencies (make-hash-table :test #'equal)))
      (dolist (line lines)
        (incf (gethash line frequencies 0)))
      (setf frequencies (sort (hash-table-alist frequencies) #'> :key #'cdr))
      (mapcar #'car (subseq frequencies 0 (min (length frequencies) max-authors))))))
