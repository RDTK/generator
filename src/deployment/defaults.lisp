;;;; defaults.lisp --- Default behavior of the deployment protocol.
;;;;
;;;; Copyright (C) 2012-2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:build-generator.deployment)

;;; Utilities

(defun deploy-job-dependencies (jobs)
  (with-sequence-progress (:deploy/dependencies jobs)
    (loop :for job :in jobs
          :do (progress "~/print-items:format-print-items/"
                        (print-items:print-items job))
              (deploy-dependencies job))))

;;; `project:distribution'

(defmethod deploy ((thing project:distribution))
  (let ((versions (project:versions thing)))
    (with-sequence-progress (:deploy/project versions)
      (mappend (lambda (version)
                 (progress "~/print-items:format-print-items/"
                           (print-items:print-items version))
                 (more-conditions::without-progress
                   (with-simple-restart
                       (continue "~@<Skip deploying project version ~S.~@:>" version)
                     (flatten (deploy version)))))
               versions))))

;;; `project:version'

(defvar *outermost-version?* t)

(defmethod deploy :around ((thing project:version))
  (if *outermost-version?*
      (with-condition-translation (((error project-deployment-error)
                                    :thing thing))
        (let ((*outermost-version?* nil))
          (call-next-method)))
      (call-next-method)))

(defmethod deploy ((thing project:version))
  (let ((jobs (project:jobs thing)))
    (with-sequence-progress (:deploy/job jobs)
      (mappend (lambda (job)
                 (progress "~/print-items:format-print-items/"
                           (print-items:print-items job))
                 (list (deploy job)))
               jobs))))

;;; `project:job'

(defmethod deploy ((thing project::job))
  (let+ ((id        (substitute-if-not
                     #\_ #'jenkins.api:job-name-character?
                     (var:value/cast thing :build-job-name)))
         (kind      (let+ (((kind &optional plugin)
                            (ensure-list (var:value thing :kind))))
                      (if plugin
                          (list kind plugin)
                          (make-keyword (string-upcase kind)))))
         (disabled? (var:value/cast thing :build-job.disabled? nil))
         ((&flet make-new-job (&optional existing-job)
            (let ((job (make-instance 'jenkins.api:job
                                      :id        id
                                      :check-id? t
                                      :kind      kind
                                      :populate? t)))
              ;; Retain value of disabled slot unless
              ;; `:force-disabled' has been specified.
              (cond
                ((eq disabled? :force-disabled)
                 (setf (jenkins.api:disabled? job) t))
                ((not existing-job)
                 (setf (jenkins.api:disabled? job) disabled?))
                (t
                 (setf (jenkins.api:disabled? job) (jenkins.api:disabled? existing-job))))

              (push job (model:implementations thing))

              ;; Apply aspects, respecting declared ordering, and sort
              ;; generated builders according to declared ordering.
              (aspects:extend! job (aspects:aspects thing) thing)

              ;; TODO temp
              (xloc:->xml job (stp:root (jenkins.api::%data job)) 'jenkins.api:job)

              job))))

    ;; Create the actual Jenkins job.
    (let* ((existing-job  (when (jenkins.api:job? id)
                            (jenkins.api:job id)))
           (existing-kind (when existing-job
                            (jenkins.api:kind existing-job))))
      (cond
        ((not existing-job)
         (let ((new-job (make-new-job)))
           (log:info "~@<Creating new job ~A~@:>" new-job)
           (jenkins.api:make-job id (jenkins.api::%data new-job))))

        ((or (equal kind existing-kind)
             (case kind
               (:project (string= existing-kind "project"))
               (:matrix  (string= existing-kind "matrix-project"))))
         (let ((new-job (make-new-job existing-job)))
           (log:info "~@<Updating existing job ~A~@:>" existing-job)
           (setf (jenkins.api:job-config id)
                 (jenkins.api::%data new-job))))

        (t
         (let ((new-job (make-new-job existing-job)))
           (log:warn "~@<Deleting job ~A to change kind ~A -> ~(~A~)~@:>"
                     new-job (jenkins.api:kind existing-job) kind)
           (jenkins.api:delete-job id)
           (jenkins.api:make-job id (jenkins.api::%data new-job))))))

    thing))

(defmethod deploy-dependencies ((thing project::job))
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
        (trigger (jenkins.api::trigger/reverse))
      (setf (jenkins.api::threshold trigger) required-upstream-result))
    ;; Install relations between JOB and its upstream jobs.
    (loop :for upstream-job :in relevant-dependencies
          :do (with-simple-restart (continue "~@<Do not relate ~A -> ~A~@:>"
                                             upstream-job thing)
                (handler-bind
                    ((error (lambda (condition)
                              (error "~@<Could not relate ~A -> ~A: ~A.~@:>"
                                     upstream-job thing condition))))
                  (jenkins.api:relate (model:implementation upstream-job) job
                                      :if-related nil))))))
