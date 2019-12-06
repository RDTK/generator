;;;; target.lisp --- Deployment target for Jenkins.
;;;;
;;;; Copyright (C) 2018, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:build-generator.deployment.jenkins)

(defclass target ()
  ((%delete-other?        :initarg :delete-other?
                          :reader  delete-other?)
   (%delete-other-pattern :initarg :delete-other-pattern
                          :reader  delete-other-pattern)))

(service-provider:register-provider/class
 'deploy:target :jenkins :class 'target)

(defmethod deploy:deploy ((thing sequence) (target target))
  (unless (every (of-type 'project:distribution) thing)
    (return-from deploy:deploy (call-next-method)))

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
         thing jenkins-jobs (delete-other-pattern target))))))

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
