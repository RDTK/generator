;;;; classes-model.lisp --- Classes modeling projects, versions and jobs.
;;;;
;;;; Copyright (C) 2012-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

;;; The object-aggregation hierarchy is as follows:
;;;
;;; job
;;;   aspect

(cl:in-package #:jenkins.model.project)

;;; `job' class

(defclass job (named-mixin
               implementation-mixin
               specification-mixin ; TODO define another class for this
               parented-mixin
               direct-variables-mixin)
  ((direct-dependencies :initarg  :direct-dependencies ; TODO(jmoringe, 2013-03-06): dependencies-mixin?
                        :type     list ; of job
                        :reader   direct-dependencies
                        :accessor %direct-dependencies
                        :initform '()
                        :documentation
                        "List of other `job' instances.")
   (aspects             :initarg  :aspects
                        :type     list
                        :reader   aspects
                        :initform '()
                        :documentation
                        "List of aspects associated to the job."))
  (:documentation
   "Instances of this class represent build jobs which are associated
    to specific `version's of `project's."))

(defmethod direct-variables ((thing job))
  (value-acons :job-name (name thing)
               (when (next-method-p)
                 (call-next-method))))

(defmethod add-dependencies! ((thing job) (spec job-spec)
                              &key providers)
  (declare (ignore providers))
  (let ((dependency-name (value/cast thing :dependency-job-name (name thing))))
    (iter (for dependency in (direct-dependencies (parent thing)))
          (log:trace "~@<Trying to add ~A to ~A~@:>" dependency thing)
          (with-simple-restart (continue "~@<Skip adding dependency ~A.~@:>"
                                         dependency)
            (let ((dependency
                   (or (find dependency-name (jobs dependency)
                             :test #'string= :key #'name)
                       (error "~@<Could not find ~S in the jobs of ~
                               ~A~@[ (~{~A~^, ~})~]~@:>"
                              dependency-name dependency (jobs dependency)))))
              (pushnew dependency (%direct-dependencies thing)))))))

(defmethod deploy ((thing job))
  (let+ ((id        (substitute-if-not
                     #\_ #'jenkins.api:job-name-character?
                     (value/cast thing :build-job-name)))
         (disabled? (value/cast thing :build-job.disabled? nil))
         (kind      (let+ (((kind &optional plugin)
                            (ensure-list (value thing :kind))))
                      (if plugin
                          (list kind plugin)
                          (make-keyword (string-upcase kind)))))
         ((&flet make-new-job (new?)
            (let ((job (jenkins.dsl:job (kind id :disabled? (if new?
                                                                disabled?
                                                                nil)))))
              (push job (jenkins.model:implementations thing))

              ;; Apply aspects, respecting declared ordering, and sort
              ;; generated builders according to declared ordering.
              (jenkins.model.aspects:extend! job (aspects thing) thing)

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
         (let ((new-job (make-new-job t)))
           (log:info "~@<Creating new job ~A~@:>" new-job)
           (jenkins.api:make-job id (jenkins.api::%data new-job))))

        ((or (equal kind existing-kind)
             (case kind
               (:project (string= existing-kind "project"))
               (:matrix  (string= existing-kind "matrix-project"))))
         (let ((new-job (make-new-job nil)))
           (log:info "~@<Updating existing job ~A~@:>" existing-job)
           (setf (jenkins.api:job-config id)
                 (jenkins.api::%data new-job))))

        (t
         (let ((new-job (make-new-job t)))
           (log:warn "~@<Deleting job ~A to change kind ~A -> ~(~A~)~@:>"
                     new-job (jenkins.api:kind existing-job) kind)
           (jenkins.api:delete-job id)
           (jenkins.api:make-job id (jenkins.api::%data new-job))))))

    thing))

(defmethod deploy-dependencies ((thing job))
  (let ((relevant-dependencies
         (ecase (value/cast thing :dependencies.mode :direct)
           (:direct  (direct-dependencies thing))
           (:minimal (minimal-dependencies thing))
           (:none    '()))))
    (iter (for upstream-job in relevant-dependencies)
          (with-simple-restart (continue "~@<Do not relate ~A -> ~A~@:>"
                                         upstream-job thing)
            (handler-bind
                ((error (lambda (condition)
                          (error "~@<Could not relate ~A -> ~A: ~A.~@:>"
                                 upstream-job thing condition))))
              (jenkins.api:relate (implementation upstream-job) (implementation thing)
                                  :if-related nil))))))
