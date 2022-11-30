;;;; target.lisp --- Deployment target for Jenkins.
;;;;
;;;; Copyright (C) 2018, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:build-generator.deployment.jenkins)

(defclass target ()
  ((base-uri             :initarg  :base-uri
                         :type     puri:uri
                         :reader   base-uri
                         :initform (puri:uri "https://localhost:8080")
                         :documentation
                         "Jenkins base URI.")
   (username             :initarg  :username
                         :type     (or null string)
                         :reader   username
                         :initform nil
                         :documentation
                         "Username for Jenkins authentication.")
   (password             :initarg  :password
                         :type     (or null string)
                         :reader   password
                         :initform nil
                         :documentation
                         "Password for Jenkins authentication.")
   (api-token            :initarg  :api-token
                         :type     (or null string)
                         :reader   api-token
                         :initform nil
                         :documentation
                         "API token for Jenkins authentication.")
   (delete-other?        :initarg  :delete-other?
                         :type     boolean
                         :reader   delete-other?
                         :initform nil
                         :documentation
                         #.(format nil "Delete previously ~
                            automatically generated jobs when they ~
                            are not re-created in this generation ~
                            run."))
   (delete-other-pattern :initarg  :delete-other-pattern
                         :type     (or null string)
                         :reader   delete-other-pattern
                         :initform nil
                         :documentation
                         #.(format nil "When deleting previously ~
                            automatically generated jobs, only ~
                            consider jobs whose name matches the ~
                            regular expression REGEX.~@
                            ~@
                            The default value corresponds to the ~
                            common case of deleting only jobs ~
                            belonging to previous versions of the ~
                            distribution(s) being generated, i.e. the ~
                            regular expression ~
                            (DISTRIBUTION-NAME₁|DISTRIBUTION-NAME₂|…)$.")))
  (:documentation
   "Generate Jenkins jobs for given distribution(s)."))

(service-provider:register-provider/class
 'deploy:target :jenkins :class 'target)

(defmethod deploy:deploy ((thing sequence) (target target))
  (unless (every (of-type 'project:distribution) thing)
    (return-from deploy:deploy (call-next-method)))

  (let+ (((&accessors-r/o base-uri username password api-token) target)
         (credentials (cond (api-token
                             (jenkins.api:make-token-credentials
                              username api-token))
                            ((and username password)
                             (jenkins.api:make-username+password-credentials
                              username password))))
         (endpoint    (jenkins.api:make-endpoint
                       base-uri :credentials credentials)))
    (progn ; TODO as-phase (:verify-jenkins)
      (jenkins.api::verify-jenkins :endpoint endpoint))

    (jenkins.api:with-endpoint (endpoint)
      (let+ (((jobs orchestration-jobs &ign)
              (reduce (lambda+ ((all-jobs all-orchestration-jobs all-views) distribution)
                        (let+ (((&values jobs orchestration-jobs views)
                                (deploy:deploy distribution target)))
                          (list (append all-jobs               jobs)
                                (append all-orchestration-jobs orchestration-jobs)
                                (append all-views              views))))
                      thing :initial-value '(() () ())))
             (all-jobs     (append jobs orchestration-jobs))
             (jenkins-jobs (mappend #'model:implementations all-jobs)))
        (when (delete-other? target)
          (with-simple-restart (continue "~@<Do not delete other jobs~@:>")
            (delete-other-jobs
             thing jenkins-jobs (delete-other-pattern target))))

        (progn ; TODO as-phase (:list-credentials)
          (list-credentials jenkins-jobs))))))

;;; Deleting jobs

(defun generated? (job)
  (search "automatically generated" (jenkins.api:description job)))

(defun %delete-other-jobs (all-jobs pattern)
  (log:info "Deleting other jobs using pattern ~S" pattern)
  (when-let* ((other-jobs     (set-difference
                               (jenkins.api:all-jobs pattern) all-jobs
                               :key #'jenkins.api:id :test #'string=))
              (generated-jobs (remove-if-not #'generated? other-jobs)))
    (with-sequence-progress (:delete-other generated-jobs)
      (map nil (lambda (job)
                 (progress "~A" (jenkins.api:id job))
                 (jenkins.api:delete-job job))
           generated-jobs))))

(defun make-delete-other-pattern (pattern distributions)
  (cond (pattern)
        ((length= 1 distributions)
         `(:sequence ,(model:name (first-elt distributions)) :end-anchor))
        (t
         `(:sequence
           (:alternation ,@(map 'list #'model:name distributions))
           :end-anchor))))

(defun delete-other-jobs (distributions all-jobs pattern)
  (when (or pattern (not (emptyp distributions)))
    (%delete-other-jobs all-jobs (make-delete-other-pattern
                                  pattern distributions))))

;;; Credentials

(defun list-credentials (jobs)
  (let+ ((all-credentials (make-hash-table :test #'equal))
         ((&flet+ job-credentials (job)
            (when-let* ((repository  (jenkins.api:repository job))
                        (credentials (jenkins.api:credentials repository)))
              (push job (gethash credentials all-credentials))))))
    (mapc #'job-credentials jobs)
    (when (plusp (hash-table-count all-credentials))
      (format t "~@<The following credentials have been referenced and ~
                 have to be configured in Jenkins' credential store:~@:_~
                 ~{~{* ~S for job~P ~<~{~A~^, ~}~:@>~}~^~@:_~}~
                 ~%~:>"
              (mapcar (lambda+ ((credentials . jobs))
                        (list credentials (length jobs)
                              (list (mapcar #'jenkins.api:id jobs))))
                      (hash-table-alist all-credentials))))))
