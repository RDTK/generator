;;;; mercurial.lisp --- Support for the mercurial DVCS.
;;;;
;;;; Copyright (C) 2014, 2015, 2017 Jan Moringen
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
         ((&flet analyze-directory (version directory)
            (apply #'analyze directory :auto
                   (append
                    (remove-from-plist version :branch :tag :commit)
                    (remove-from-plist args :username :password :versions
                                            :sub-directory :history-limit
                                            :temp-directory))))))

    (unwind-protect
         (progn
           (with-trivial-progress (:clone "~A" repository/string)
             (%run-mercurial
              `("clone" ,repository/string ,clone-directory)
              temp-directory))

           (with-sequence-progress (:analyze/version versions)
             (iter (for version in versions)
                   (progress "~A" version)
                   (with-simple-restart
                       (continue "~<Ignore ~A and continue with the ~
                                  next version.~@:>"
                                 version)
                       (let ((commit (or (getf version :commit)
                                         (getf version :tag)
                                         (getf version :branch)
                                         (error "~@<No commit, tag or ~
                                                 branch specified in ~
                                                 ~S~@:>"
                                                version))))

                         (%run-mercurial `("checkout" ,commit) clone-directory)

                         (let ((result     (analyze-directory version analyze-directory))
                               (committers (analyze clone-directory :mercurial/committers)))
                           (collect (list* :scm              :mercurial
                                           :branch-directory nil
                                           :committers       committers
                                           result))))))))

      (when (probe-file clone-directory)
        (run `("rm" "-rf" ,clone-directory) *default-pathname-defaults*)))))

(defmethod analyze ((directory pathname) (kind (eql :mercurial/committers))
                    &key
                    (max-committers 5))
  (with-trivial-progress (:analyze/log "~A" directory)
    (let* ((lines
             (apply #'inferior-shell:run/nil
                    `("hg" "log" "--template" "{author}\\n")
                    :directory directory
                    :output    :lines
                    (safe-external-format-argument)))
           (person-collector (make-names->person-list :count max-committers)))
      (map nil (lambda (line)
                 (funcall person-collector
                          (string-trim '(#\Tab #\Space #\" #\') line)))
           lines)
      (funcall person-collector))))
