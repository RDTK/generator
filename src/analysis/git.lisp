;;;; git.lisp ---
;;;;
;;;; Copyright (C) 2012, 2013, 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.analysis)

(define-constant +ssh-askpass-variable-name+
  "SSH_ASKPASS"
  :test #'string=)

(define-constant +disable-git-credentials-helper-program+
  "true"
  :test #'string=)

(defun environment-with-ssh-askpass-overwritten ()
  (list* (format nil "~A=~A"
                 +ssh-askpass-variable-name+
                 +disable-git-credentials-helper-program+)
         (remove-if (curry #'starts-with-subseq
                           +ssh-askpass-variable-name+)
                    (sb-ext:posix-environ))))

(defun %run-git (spec directory &key non-interactive)
  (let+ (((&values global-options environment)
          (if non-interactive
              `("-c" ,(format nil "core.askpass=~A"
                              +disable-git-credentials-helper-program+))
              (values () (environment-with-ssh-askpass-overwritten)))))
    (run `("git" ,@global-options  ,@spec)
         directory :environment environment)))

(defmethod analyze ((source puri:uri) (schema (eql :git))
                    &rest args &key
                    username
                    password
                    (versions       (missing-required-argument :versions))
                    sub-directory
                    history-limit
                    (temp-directory (default-temporary-directory))
                    non-interactive)
  ;;; TODO(jmoringe, 2013-01-17): find branches automatically
  (let+ ((repository/string (format-git-url source username password))
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
             (%run-git
              `("clone" "--quiet"
                ,@(when history-limit `("--depth" ,history-limit))
                ,repository/string ,clone-directory)
              temp-directory :non-interactive non-interactive))

           (with-sequence-progress (:analyze/version versions)
             (iter (for version in versions)
                   (progress "~A" version)

                   (restart-case
                       (let ((commit (or (getf version :commit)
                                         (getf version :tag)
                                         (getf version :branch)
                                         (error "~@<No commit, tag or ~
                                                 branch specified in ~
                                                 ~S~@:>"
                                                version))))
                         (%run-git
                          `("--work-tree" ,clone-directory
                            "--git-dir" ,(merge-pathnames ".git/" clone-directory)
                            "checkout" "--quiet" ,commit)
                          clone-directory)

                         (let ((result (list* :scm              :git
                                              :branch-directory nil
                                              (analyze-directory analyze-directory))))
                           (unless (getf result :authors)
                             (setf (getf result :authors)
                                   (analyze clone-directory :git/authors)))
                           (collect (cons version result))))
                     (continue (&optional condition)
                       :report (lambda (stream)
                                 (format stream "~<Ignore ~A and ~
                                                 continue with the next ~
                                                 version.~@:>"
                                         version))
                       (declare (ignore condition)))))))

      (when (probe-file clone-directory)
        (run `("rm" "-rf" ,clone-directory) temp-directory)))))

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
