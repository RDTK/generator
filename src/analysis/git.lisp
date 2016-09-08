;;;; git.lisp ---
;;;;
;;;; Copyright (C) 2012, 2013, 2014, 2015, 2016 Jan Moringen
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
    (run `("git" ,@global-options ,@spec)
         directory :environment environment)))

(defun clone-git-repository (source clone-directory
                             &key
                             username
                             password
                             branch
                             commit
                             mirror?
                             history-limit
                             non-interactive)
  (assert (not (and branch commit)))
  (let ((repository/string (format-git-url source username password)))
    (with-trivial-progress (:clone "~A ~A -> ~A"
                                   repository/string branch clone-directory)
      (%run-git
       `("clone" "--quiet"
                 ,@(when branch        `("--branch" ,branch))
                 ,@(when mirror?       '("--mirror"))
                 ,@(when history-limit `("--depth" ,history-limit))
                 ,repository/string ,clone-directory)
       "/" :non-interactive non-interactive)
      (when commit
        (%run-git `("checkout" ,commit)
                  clone-directory :non-interactive non-interactive)))))

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
                                    commit
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
                          :commit          commit
                          :history-limit   history-limit
                          :non-interactive t)))

(defun clone-git-repository/maybe-cached (source clone-directory
                                          &rest args &key
                                          cache-directory
                                          &allow-other-keys)
  (let+ (((&values function args)
          (if cache-directory
              (values #'clone-git-repository/cached args)
              (values #'clone-git-repository
                      (remove-from-plist args :cache-directory)))))
    (apply function source clone-directory args)
    (git-directory-commit-key clone-directory)))

(defun analyze-git-branch (clone-directory
                           &rest args
                           &key sub-directory &allow-other-keys)
  (let* ((analyze-directory (if sub-directory
                                (merge-pathnames sub-directory clone-directory)
                                clone-directory))
         (result            (apply #'analyze analyze-directory :auto
                                   (remove-from-plist args :sub-directory)))
         (authors           (or (getf result :authors)
                                (analyze clone-directory :git/authors))))
    (list* :scm              :git
           :branch-directory nil
           :authors          authors
           result)))

(defun analyze-git-branch/cached (cache-directory key)
  (with-simple-restart (continue "~@<Do not use cache results.~@:>")
    (let ((file (merge-pathnames key cache-directory)))
      (log:info "~@<Maybe restoring analysis results in ~A~@:>" file)
      (when (probe-file file)
        (log:info "~@<Restoring analysis results in ~A~@:>" file)
        (cl-store:restore file)))))

(defun analyze-git-branch/cache (cache-directory key results)
  (with-simple-restart (continue "~@<Do not cache results.~@:>")
    (let ((file (merge-pathnames key cache-directory)))
      (log:info "~@<Storing analysis results in ~A~@:>" file)
      (cl-store:store results file))))

(defun analyze-git-branch/maybe-cached (clone-directory
                                        &rest args &key
                                        cache-directory
                                        key
                                        &allow-other-keys)
  (or (when (and cache-directory key)
        (analyze-git-branch/cached cache-directory key))
      (let ((results (apply #'analyze-git-branch clone-directory
                            (remove-from-plist
                             args
                             :commit :branch :scm :username :password
                             :history-limit :non-interactive
                             :cache-directory :key))))
        (when (and cache-directory key)
          (analyze-git-branch/cache cache-directory key results))
        results)))

(defun clone-and-analyze-git-branch (source clone-directory
                                     &rest args &key
                                     commit
                                     branch
                                     username
                                     password
                                     sub-directory
                                     cache-directory
                                     non-interactive
                                     &allow-other-keys)
  ;; If we already have analysis results for the commit that is
  ;; current in the remote repository, return right away.
  (with-simple-restart (continue "~@<Do not try to use cached analysis ~
                                  results.~@:>")
    (when cache-directory
      (log:info "~@<Determining current remote commit in ~A~@:>" source)
      (when-let* ((commitish  (or commit branch))
                  (commit-key (git-remote-commit-key
                               source commitish
                               :username        username
                               :password        password
                               :non-interactive non-interactive))
                  (key        (%sub-directory->key commit-key sub-directory))
                  (results    (analyze-git-branch/cached
                               cache-directory key)))
        (return-from clone-and-analyze-git-branch results))))

  ;; Clone the repository, then analyze the requested
  ;; branches/tags/commits, potentially caching the results.
  (let* ((commit-key (clone-git-repository/maybe-cached
                      source clone-directory
                      :commit          commit
                      :branch          branch
                      :username        username
                      :password        password
                      :cache-directory cache-directory
                      :non-interactive non-interactive))
         (key        (%sub-directory->key commit-key sub-directory)))
    (log:info "~@<Cloned ~A into ~A, got key ~A~@:>"
              source clone-directory key)
    (apply #'analyze-git-branch/maybe-cached clone-directory
           :key key
           args)))

(defmethod analyze ((source puri:uri) (schema (eql :git))
                    &rest args &key
                    (versions       (missing-required-argument :versions))
                    (temp-directory (default-temporary-directory)))
  (let+ (((&flet find-commitish (version)
            (or (when-let ((commit (getf version :commit)))
                  (list :commit commit))
                (when-let ((branch (or (getf version :tag)
                                       (getf version :branch))))
                  (list :branch branch))
                (error "~@<No commit, tag or branch specified in ~
                        ~S~@:>"
                       version))))
         ((&flet make-clone-directory (version)
            (let ((version (format nil "~(~{~A~^-~}~)" version)))
              (merge-pathnames
               (concatenate
                'string (ppcre:regex-replace-all "/" version "_") "/")
               temp-directory))))
         ((&flet analyze-version (version)
            (unwind-protect
                 (let ((commitish (find-commitish version)))
                   (cons version
                         (apply #'clone-and-analyze-git-branch
                                source (make-clone-directory commitish)
                                (append commitish
                                        (remove-from-plist
                                         args :versions :temp-directory)))))

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

;;; Utilities

(defun git-remote-commit-key (source branch
                              &key
                              username
                              password
                              non-interactive)
  (let* ((url    (format-git-url source username password))
         (deref  (format nil "~A^{}" branch))
         (output (%run-git `("ls-remote" ,url ,branch ,deref) "/"
                           :non-interactive non-interactive))
         (result))
    (ppcre:do-register-groups (commit ref deref?)
        ((ppcre:create-scanner "^([a-z0-9]+)\\t(.*?)(\\^\\{\\})?$"
                               :multi-line-mode t)
         output)
      (let+ (((&optional result-commit result-tag? result-deref?) result)
             (tag? (starts-with-subseq "refs/tags/" ref)))
       (when (or (not result-commit)               ; anything > nothing
                 (and result-tag? (not tag?))      ; not tag  > tag
                 (and result-tag? tag?             ; when both tags:
                      (not result-deref?) deref?)) ; deref    > not deref
         (setf result (list commit tag? deref?)))))
    (when result
      (%git-commit->key (first result)))))

(defun git-directory-commit-key (directory)
  (let ((commit (apply #'inferior-shell:run/nil ; TODO capture error output?
                       '("git" "rev-parse" "HEAD")
                       :output    '(:string :stripped t)
                       :directory directory
                       (safe-external-format-argument))))
    (%git-commit->key commit)))

(defun %git-commit->key (commit)
  (format nil "git:~A" commit))

(defun %sub-directory->key (key sub-directory)
  (if sub-directory
      (let* ((octets (sb-ext:string-to-octets
                      (namestring sub-directory) :external-format :utf-8))
             (hash   (ironclad:digest-sequence 'ironclad:sha256 octets)))
        (format nil "~A:~(~{~2,'0X~}~)" key (coerce hash 'list)))
      key))
