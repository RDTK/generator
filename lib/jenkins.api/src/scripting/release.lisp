;;;; release.lisp ---
;;;;
;;;; Copyright (C) 2012, 2013, 2014, 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.scripting)

(defmacro with-modification ((label old &optional (new old))
                             &body body)
  (with-gensyms (temp)
    `(let ((,temp ,old))
       (prog1
           (progn ,@body)
         (format t "~2@T~24A ~@<~A -> ~A~:@>~%" ,label ,temp ,new)))))

(defun adapt (job version)
  (macrolet ((regex-replacef (pattern place replacement)
               `(setf ,place (ppcre:regex-replace ,pattern ,place ,replacement)))
             (regex-modification (label place pattern replacement)
               `(with-modification (,label ,place)
                  (regex-replacef ,pattern ,place ,replacement))))

    ;; Modify description.
    (when (description job)
      (with-modification (:description "(t|T)runk" version)
        (regex-replacef "(t|T)runk" (description job) version)))

    ;; Modify repository URLs.
    (let ((repo (repository job)))
      (typecase repo
        (scm/svn
         (with-modification (:svn (url repo))
           (regex-replacef "trunk" (url repo) (format nil "branches/~A" version))))
        (scm/git
         (with-modification (:git/branch (first (branches repo)))
           (regex-replacef "master" (first (branches repo)) version))
         (with-modification (:git/local-branch (local-branch repo))
           (setf (local-branch repo) version)))))

    ;; Modify environment variables.
    (setf (environment job)
          (iter (for (name value) on (environment job) :by #'cddr)
                (case name
                  (:package_revision
                   (collect name)
                   (regex-modification
                    (format-symbol :keyword "~A/~A" :environment name)
                    value "^-b(.*)$" "-r\\1")
                   (collect value))
                  (t
                   (collect name)
                   (collect value)))))

    ;; Modify source projects of copy artifact build steps.
    (dolist (builder (builders job))
      (typecase builder
        (builder/copy-artifact
         (with-modification (:builder (project-name builder))
           (regex-replacef "(rs(?:b|c|t).*)-trunk" (project-name builder)
                           (format nil "\\1-~A" version))))))

    ;; Update ssh upload target directories.
    (dolist (upload (publishers-of-type 'publisher/ssh job))
      (with-modification (:ssh-upload (remote-directory upload))
        (regex-replacef "trunk"          (remote-directory upload) version)
        (regex-replacef "upload-testing" (remote-directory upload) "upload")))

    job))

(defun adapt-relations (job version)
  ;; Modify source projects of copy artifact build steps.
  (dolist (upstream/old (jenkins.api::upstream job))
    (restart-case
        (let ((upstream/new
                (ppcre:regex-replace "(rs(?:b|c|t).*)-trunk" upstream/old
                                     (format nil "\\1-~A" version))))
          (when (string/= upstream/old upstream/new)
            (with-modification (:upstream upstream/old upstream/new)
              (progn #+true when #+true (member upstream/old (jenkins.api::upstream job) :test #'string=)
                     (unrelate upstream/old job))
              (unless (member upstream/new (jenkins.api::upstream job) :test #'string=)
                (relate upstream/new job)))))
      (continue (&optional condition)
        :report (lambda (stream)
                  (format stream "~@<Skip adapting the relation ~A - ~A.~@:>"
                          job upstream/old))
        (log:error "~@<Error adapting relation ~A - ~A~@[: ~A~].~@:>"
                   job upstream/old condition)))))

(defun release (version
                &key
                (job-pattern     "-trunk")
                blacklist
                (version-pattern job-pattern)
                (replace-pattern version-pattern)
                (replacement     (format nil "-~A" version)))
  (let ((jobs (remove-if (lambda (job)
                           (etypecase blacklist
                             (null nil)
                             (function (funcall blacklist job))))
                         (all-jobs job-pattern)))
        (jobs/new ()))

    (log:info "~@<Copying ~:D job~:P~@:>" (length jobs))
    (dolist (job jobs)
      (restart-case
          (let* ((new/name (ppcre:regex-replace
                            replace-pattern (id job) replacement))
                 (messages (make-string-output-stream))
                 (new/job  (copy-job/fixup (id job) new/name)))
            (format t "~A -> ~A~%~<  ~:;~A~:>~%"
                    job new/job
                    (list (get-output-stream-string messages)))
            (adapt new/job version)
            (commit! new/job)
            (log:debug (stp:serialize (jenkins.api::%data new/job)
                                      (cxml:make-string-sink :indentation 2)))
            (push new/job jobs/new))
        (continue (&optional condition)
          :report (lambda (stream)
                    (format stream "~@<Skip copying job ~A.~@:>" job))
          (log:error "~@<Error in ~A~@[: ~A~]~@:>" job condition))))

    (log:info "~@<Adjusting relationships~@:>")
    (dolist (job (append jobs/new jobs/new)) ; needs two passes?
      (restart-case
          (progn
            (format t "~A~%" job)
            (adapt-relations job version))
        (continue (&optional condition)
          :report (lambda (stream)
                    (format stream "~@<Skip adapting relations of job ~A.~@:>" job))
          (log:error "~@<Error in ~A~@[: ~A~]~@:>" job condition))))))

;;; Example

#+no (release "0.10" :job-pattern     "^rs(c|b|t).*-trunk"
                     :blacklist       (lambda (job)
                                        (ppcre:scan "(merge-simulator$|performance-monitor|performance-test|coordination|scxml)"
                                                    (id job)))
                     :version-pattern "-trunk")
