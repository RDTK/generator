;;;; mercurial.lisp --- Support for the mercurial DVCS.
;;;;
;;;; Copyright (C) 2014-2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.analysis)

(defun %run-mercurial (spec directory)
  (run `("hg" :noninteractive ; :color "never" :pager "never"
              ,@spec)
       directory))

(defun mercurial-clone (repository directory temp-directory &key disable-bundles?)
  (with-condition-translation (((error repository-access-error)
                                :specification repository))
    (restart-case
        (%run-mercurial
         `("clone" ,@(when disable-bundles? '(:config "ui.clonebundles=false"))
                   ,repository ,directory)
         temp-directory)
      (continue (&optional condition)
        :test (lambda (condition)
                (declare (ignore condition))
                (not disable-bundles?))
        :report "Retry cloning with bundles disabled"
        (declare (ignore condition))
        ;; Mercurial deletes the directory if the clone operation
        ;; fails. Restore it before retrying. Race conditions? Security
        ;; considerations? Look! Over there, a three-headed monkey!
        (ensure-directories-exist temp-directory)
        (mercurial-clone repository directory temp-directory
                         :disable-bundles? t)))))

(defmethod analyze ((source puri:uri) (schema (eql :mercurial))
                    &rest args &key
                    username
                    password
                    (versions (missing-required-argument :versions))
                    sub-directory
                    (temp-directory (util:default-temporary-directory)))
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
             (mercurial-clone
              repository/string clone-directory temp-directory))

           (when sub-directory
             (with-condition-translation (((error repository-access-error)
                                           :specification source))
               (check-repository-sub-directory clone-directory sub-directory)))

           (with-sequence-progress (:analyze/version versions)
             (iter (for version in versions)
                   (progress "~A" version)
                   (with-simple-restart
                       (continue "~@<Ignore ~A and continue with the ~
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
                               (committers (with-simple-restart
                                               (continue "~@<Do not analyze committers in ~A.~@:>"
                                                         clone-directory)
                                             (analyze clone-directory :mercurial/committers))))
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
                    (util:safe-external-format-argument)))
           (person-collector (make-names->person-list :count max-committers)))
      (map nil (lambda (line)
                 (funcall person-collector
                          (string-trim '(#\Tab #\Space #\" #\') line)))
           lines)
      (funcall person-collector))))
