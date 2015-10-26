;;;; mercurial.lisp --- Support for the mercurial DVCS.
;;;;
;;;; Copyright (C) 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.analysis)

(defun %run-mercurial (spec directory)
  (run `("hg" ,@spec) directory))

(defmethod analyze ((source puri:uri) (schema (eql :mercurial))
                    &rest args &key
                    username
                    password
                    (versions (missing-required-argument :versions))
                    sub-directory
                    (temp-directory (default-temporary-directory)))
  (when (or username password)
    (error "~@<Authentication support not implemented for ~A.~@:>" schema))

  (let+ ((repository/string (princ-to-string source))
         (clone-directory   temp-directory)
         (analyze-directory (if sub-directory
                                (merge-pathnames sub-directory clone-directory)
                                clone-directory))
         ((&flet analyze-directory (directory)
            (apply #'analyze directory :auto
                   (remove-from-plist args :username :password :versions
                                           :sub-directory :history-limit
                                           :temp-directory)))))

    (unwind-protect
         (progn
           (with-trivial-progress (:clone "~A" repository/string)
             (%run-mercurial
              `("clone" ,repository/string ,clone-directory)
              temp-directory))

           (with-sequence-progress (:analyze/version versions)
             (iter (for version in versions)
                   (progress "~A" version)

                   (restart-case
                       (progn
                         (let ((commit (or (getf version :commit)
                                           (getf version :tag)
                                           (getf version :branch)
                                           (error "~@<No commit, tag or ~
                                                 branch specified in ~
                                                 ~S~@:>"
                                                  version))))

                           (%run-mercurial `("checkout" ,commit) clone-directory)

                           (let ((result (list* :scm              :mercurial
                                                :branch-directory nil
                                                (analyze-directory analyze-directory))))
                             (unless (getf result :authors)
                               (setf (getf result :authors)
                                     (analyze clone-directory :mercurial/authors)))
                             (collect (cons version result)))))
                     (continue (&optional condition)
                       :report (lambda (stream)
                                 (format stream "~<Ignore ~A and ~
                                                 continue with the ~
                                                 next version.~@:>"
                                         version))
                       (declare (ignore condition)))))))

      (when (probe-file clone-directory)
        (run `("rm" "-rf" ,clone-directory) *default-pathname-defaults*)))))

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
