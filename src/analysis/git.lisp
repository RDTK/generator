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

(defun clone-git-repository (source clone-directory
                             &key
                             username
                             password
                             branch
                             mirror?
                             history-limit
                             non-interactive)
  (let ((repository/string (format-git-url source username password)))
    (with-trivial-progress (:clone "~A ~A -> ~A"
                                   repository/string branch clone-directory)
      (%run-git
       `("clone" "--quiet"
                 ,@(when branch        `("--branch" ,branch))
                 ,@(when mirror?       '("--mirror"))
                 ,@(when history-limit `("--depth" ,history-limit))
                 ,repository/string ,clone-directory)
       "/" :non-interactive non-interactive))))

(defun update-git-repository (directory &key non-interactive)
  (handler-case
      (with-trivial-progress (:pull "~A" directory)
        (%run-git `("fetch" "--quiet" "--all")
                  directory :non-interactive non-interactive)
        t)
    (error (condition)
      (warn "~@<Could not update repository in directory ~A: ~A.~@:>"
            directory condition)
      nil)))

(defun ensure-updated-git-repository (source directory
                                      &key
                                      username
                                      password
                                      non-interactive)
  (flet ((clone ()
           (clone-git-repository source directory
                                 :username        username
                                 :password        password
                                 :mirror?         t
                                 :non-interactive non-interactive)))
    (cond
      ;; If the directory does not exist, we have to perform a fresh
      ;; clone in any case.
      ((not (probe-file (merge-pathnames "config" directory)))
       (clone))
      ;; If the directory exists and appears to be a repository, try
      ;; fetching.
      ((update-git-repository directory :non-interactive non-interactive))
      ;; If fetching fails, try to start over.
      (t
       (with-trivial-progress (:re-clone "~A" source)
         (with-trivial-progress (:delete "~A" directory)
           (uiop:delete-directory-tree directory :validate (constantly t)))
         (clone))))))

(defstruct (git-cache
            (:constructor make-git-cache ())
            (:copier      nil))
  (lock  (bt:make-lock "git cache")      :read-only t)
  (table (make-hash-table :test #'equal) :read-only t))

(defvar *git-cache* nil)

(defun call-with-git-cache (thunk)
  (let ((*git-cache* (make-git-cache)))
    (funcall thunk)))

(defmacro with-git-cache (() &body body)
  `(call-with-git-cache (lambda () ,@body)))

(defun make-git-cache-directory (source username cache-directory)
  (let* ((repository/string   (format-git-url source username))
         (sub-directory       (drakma:url-encode repository/string :utf-8))
         (cache-sub-directory (make-pathname
                               :directory (append (pathname-directory
                                                   cache-directory)
                                                  (list sub-directory))))
         (cache-url-path      (drakma:url-encode
                               (uiop:native-namestring cache-sub-directory)
                               :utf-8))
         (cache-url           (make-instance 'puri:uri
                                             :scheme :file
                                             :path   cache-url-path)))
    (values cache-sub-directory cache-url)))

(defun clone-git-repository/cached (source clone-directory
                                    &key
                                    username
                                    password
                                    branch
                                    history-limit
                                    cache-directory
                                    non-interactive)
  (let+ (((&values cache-sub-directory cache-url)
          (make-git-cache-directory source username cache-directory))
         ((&structure-r/o git-cache- lock table) *git-cache*))
    ;; Clone into/pull in cache. Under the lock, probe/create the
    ;; directory and make an entry in the cache. The actual repository
    ;; update happens in a delay so that the lock can be released
    ;; immediately and parallel forcing is not a problem.
    (lparallel:force
     (bt:with-lock-held (lock)
       (unless (probe-file cache-directory)
         (log:info "~@<Creating non-existent cache directory ~S.~@:>"
                   cache-directory)
         (ensure-directories-exist cache-directory))
       (ensure-gethash cache-sub-directory table
                       (lparallel:delay
                         (ensure-updated-git-repository
                          source cache-sub-directory
                          :username        username
                          :password        password
                          :non-interactive non-interactive)))))
    ;; Clone from cache. We rely on git to handle parallel clones from
    ;; one cached mirror repository.
    (clone-git-repository cache-url clone-directory
                          :branch          branch
                          :history-limit   history-limit
                          :non-interactive t)))

(defun clone-git-repository/maybe-cached (source clone-directory commit
                                          &rest args &key
                                          cache-directory
                                          &allow-other-keys)
  (apply (if cache-directory
             #'clone-git-repository/cached
             #'clone-git-repository)
         source clone-directory :branch commit
         (remove-from-plist args :cache-directory)))

(defun analyze-git-branch (clone-directory &key sub-directory)
  (let* ((analyze-directory (if sub-directory
                                (merge-pathnames sub-directory clone-directory)
                                clone-directory))
         (result            (analyze analyze-directory :auto))
         (authors           (or (getf result :authors)
                                (analyze clone-directory :git/authors))))
    (list* :scm              :git
           :branch-directory nil
           :authors          authors
           result)))

(defun clone-and-analyze-git-branch (source clone-directory commit
                                     &key
                                     username
                                     password
                                     sub-directory
                                     history-limit
                                     cache-directory
                                     non-interactive)
  ;; Clone the repository.
  (clone-git-repository/maybe-cached
   source clone-directory commit
   :username        username
   :password        password
   :history-limit   history-limit
   :cache-directory cache-directory
   :non-interactive non-interactive)
  ;; Then analyze the requested branches/tags/commits.
  (analyze-git-branch clone-directory :sub-directory sub-directory))

(defmethod analyze ((source puri:uri) (schema (eql :git))
                    &key
                    username
                    password
                    (versions       (missing-required-argument :versions))
                    sub-directory
                    history-limit
                    (temp-directory (default-temporary-directory))
                    cache-directory
                    non-interactive)
  (let+ (((&flet find-commit (version)
            (or (getf version :commit)
                (getf version :tag)
                (getf version :branch)
                (error "~@<No commit, tag or branch specified in ~
                        ~S~@:>"
                       version))))
         ((&flet make-clone-directory (version)
            (merge-pathnames
             (concatenate
              'string (ppcre:regex-replace-all "/" version "_") "/")
             temp-directory)))
         ((&flet analyze-version (version)
            (unwind-protect
                 (let ((commit (find-commit version)))
                   (cons version (clone-and-analyze-git-branch
                                  source (make-clone-directory commit) commit
                                  :username        username
                                  :password        password
                                  :sub-directory   sub-directory
                                  :history-limit   history-limit
                                  :cache-directory cache-directory
                                  :non-interactive non-interactive)))

              (when (probe-file temp-directory)
                (run `("rm" "-rf" ,temp-directory) "/"))))))
    (with-sequence-progress (:analyze/version versions)
      (mapcan
       (lambda (version)
         (progress "~A" version)
         (restart-case
             (list (analyze-version version))
           (continue (&optional condition)
             :report (lambda (stream)
                       (format stream "~<Ignore ~A and continue with ~
                                       the next version.~@:>"
                               version))
             (declare (ignore condition)))))
       versions))))

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
