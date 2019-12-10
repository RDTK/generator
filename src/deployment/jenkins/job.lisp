;;;; job.lisp --- Deployment of Jenkins jobs.
;;;;
;;;; Copyright (C) 2012-2022 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:build-generator.deployment.jenkins)

(defmethod make-job-using-job ((thing        project::job)
                               (id           t)
                               (kind         t)
                               (existing-job null))
  (jenkins.api:job id :check-id? t
                      :kind      kind
                      :populate? t))

(defmethod make-job-using-job ((thing        project::job)
                               (id           t)
                               (kind         t)
                               (existing-job t))
  (if (eq kind (jenkins.api:kind existing-job))
      existing-job
      (make-job-using-job thing id kind nil)))

(defmethod compute-job-config ((thing        project::job)
                               (id           t)
                               (kind         t)
                               (job          t)
                               (existing-job t))
  (let ((disabled? (var:value/cast thing :build-job.disabled? nil)))
    ;; Retain value of disabled slot unless `:force-disabled' has been
    ;; specified.
    (setf (jenkins.api:disabled? job)
          (cond ((eq disabled? :force-disabled)
                 t)
                ((not existing-job)
                 disabled?)
                (t
                 (jenkins.api:disabled? existing-job))))

    ;; Apply aspects, respecting declared ordering, and sort
    ;; generated builders according to declared ordering.
    (aspects:extend! (aspects:aspects thing) thing job :jenkins)

    ;; TODO temp
    (xloc:->xml job (stp:root (jenkins.api::%data job)) (type-of job))

    (jenkins.api::%data job)))

(defmethod update-job! ((thing         project::job)
                        (id            t)
                        (kind          t)
                        (config        t)
                        (job           t)
                        (existing-job  t))
  (let ((existing-kind (when existing-job
                         (jenkins.api:kind existing-job))))
    (cond ((not existing-job)
           (log:info "~@<Creating new job ~A~@:>" id)
           (jenkins.api:make-job id config))

          ((eq kind existing-kind)
           (log:info "~@<Updating existing job ~A~@:>" existing-job)
           (setf (jenkins.api:job-config id) config))

          (t
           (log:warn "~@<Deleting job ~A to change kind ~A -> ~(~A~)~@:>"
                     existing-job existing-kind kind)
           (jenkins.api:delete-job id)
           (jenkins.api:make-job id config)))))

(defmethod deploy:deploy ((thing project::job) (target target))
  (let* ((id           (jenkins-job-id thing))
         (existing-job (when (jenkins.api:job? id)
                         (jenkins.api:job id)))
         (kind         (let+ (((kind &optional plugin)
                               (ensure-list (var:value thing :kind))))
                         (if plugin
                             (list kind plugin)
                             (make-keyword (string-upcase kind)))))
         (job          (make-job-using-job thing id kind existing-job))
         (config       (compute-job-config thing id kind job existing-job)))
    (update-job! thing id kind config job existing-job)
    (push job (model:implementations thing))
    thing))

(defmethod deploy:deploy-dependencies ((thing project::job) (target target))
  (let ((relevant-dependencies
          (ecase (var:value/cast thing :dependencies.mode :direct)
            (:direct  (model:direct-dependencies thing))
            (:minimal (model:minimal-dependencies thing))
            (:none    '())))
        (required-upstream-result
          (ecase (var:value/cast thing :dependencies.required-upstream-result
                                 :success)
            (:success  :success)
            (:unstable :unstable)
            (:any      :failure)))
        (job (model:implementation thing)))
    ;; Look at required results in all upstream jobs, potentially
    ;; relaxing REQUIRED-UPSTREAM-RESULT to `:unstable' or `:failure'.
    (map nil (lambda (upstream-job)
               (ecase (var:value/cast upstream-job :dependencies.required-result
                                      :success)
                 (:success)
                 (:unstable
                  (case required-upstream-result
                    (:success
                     (setf required-upstream-result :unstable))))
                 (:any
                  (case required-upstream-result
                    ((:unstable :success)
                     (setf required-upstream-result :failure))))))
         relevant-dependencies)
    ;; Set threshold on "reverse" trigger based on required upstream
    ;; result.
    (aspects::with-interface (jenkins.api:triggers job)
        (trigger (jenkins.api:trigger/reverse))
      (setf (jenkins.api:threshold trigger) required-upstream-result))
    ;; Install relations between JOB and its upstream jobs.
    (map nil (lambda (upstream-job)
               (with-simple-restart (continue "~@<Do not relate ~A -> ~A~@:>"
                                              upstream-job thing)
                 (handler-bind
                     ((error (lambda (condition)
                               (error "~@<Could not relate ~A -> ~A: ~A.~@:>"
                                      upstream-job thing condition))))
                   (jenkins.api:relate (model:implementation upstream-job) job
                                       :if-related nil))))
         relevant-dependencies)))
