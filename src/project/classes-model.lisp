;;;; classes-model.lisp --- Classes modeling projects, versions and jobs.
;;;;
;;;; Copyright (C) 2012-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

;;; The object-aggregation hierarchy is as follows:
;;;
;;; project
;;;   version
;;;     job
;;;       aspect

(cl:in-package #:jenkins.project)

;;; `project' class

(defclass project (named-mixin
                   implementation-mixin
                   direct-variables-mixin)
  ((versions :initarg  :versions
             :type     list
             :reader   versions
             :documentation
             ""))
  (:documentation
   "Instances of this class represent projects.

    `project' instances are usually created from the specifications in
    `project-spec' instances."))

(defmethod direct-variables ((thing project))
  (value-acons :project-name (name thing)
               (when (next-method-p)
                 (call-next-method))))

(defmethod add-dependencies! ((thing project) (spec project-spec)
                              &key
                              (providers nil providers-supplied?))
  (mapc (if providers-supplied?
            (lambda (version version-spec)
              (add-dependencies! version version-spec
                                 :providers (funcall providers version-spec)))
            #'add-dependencies!)
        (versions thing) (versions spec)))

(defmethod deploy ((thing project))
  (mapcar #'deploy (versions thing)))

;;; `version' class

(defclass version (named-mixin
                   implementation-mixin
                   direct-variables-mixin
                   parented-mixin)
  ((direct-dependencies :initarg  :direct-dependencies
                        :type     list
                        :reader   direct-dependencies
                        :accessor %direct-dependencies
                        :initform '()
                        :documentation
                        "

                         either a string naming the dependency or a
                         list of the form

                           (NAME VERSION)

                         .")
   (jobs                :initarg  :jobs
                        :type     list
                        :reader   jobs
                        :documentation
                        ""))
  (:documentation
   "Instances of this class represent versions of `project's."))

(defmethod direct-variables ((thing version))
  (value-acons :version-name (name thing)
               (when (next-method-p)
                 (call-next-method))))

(defmethod add-dependencies! ((thing version) (spec version-spec)
                              &key
                              (providers nil providers-supplied?))
  (iter (for requires in (requires spec))
        (log:trace "~@<Trying to satisfy requirement ~S for ~A~:[~; ~
                    considering providers ~S~].~@:>"
                   requires thing providers-supplied? providers)
        (with-simple-restart (continue "~@<Skip requirement ~S.~@:>" requires)
          (let+ (((&flet instantiable-jobs (spec)
                    (remove-if-not (rcurry #'instantiate? spec) (jobs spec))))
                 ((&flet count-jobs (provider)
                    (length (intersection (instantiable-jobs spec)
                                          (instantiable-jobs provider)
                                          :test #'string= :key #'name))))
                 ((&flet+ order ((&ign . left-provider) (&ign . right-provider))
                    (> (count-jobs left-provider) (count-jobs right-provider))))
                 (candidate (when-let ((match (apply #'find-provider/version requires
                                                     :if-does-not-exist nil
                                                     (if providers-supplied?
                                                         (list :providers providers)
                                                         (list :order #'order)))))
                              (implementation match))))
            (cond
              (candidate
               (log:trace "~@<Best candidate is ~S.~@:>" candidate)
               (unless (eq candidate thing)
                 (pushnew candidate (%direct-dependencies thing))))
              (t
               (let ((platform-provides (platform-provides thing)))
                 (find-provider/version
                  requires :providers platform-provides)))))))

  (mapc #'add-dependencies! (jobs thing) (jobs spec)))

(defmethod deploy ((thing version))
  (mapcar #'deploy (jobs thing)))

;;; `job' class

(defclass job (named-mixin
               implementation-mixin
               specification-mixin ; TODO define another class for this
               direct-variables-mixin
               parented-mixin)
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
                        ""))
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
  (let ((dependency-name (as (value thing :dependency-job-name (name thing)) 'string)))
    (iter (for dependency in (direct-dependencies (parent thing)))
          (log:trace "~@<Trying to add ~A to ~A~@:>" dependency thing)
          (with-simple-restart (continue "~@<Skip adding dependency ~A.~@:>"
                                         dependency)
            (let ((dependency
                   (or (find dependency-name (jobs dependency)
                             :test #'string= :key #'name)
                       (error "~@<Could not find ~S in the jobs of ~
                                  ~A~@[ (~{~A~^, ~})~]~@:>"
                              dependency-name dependency
                              (jobs dependency)))))
              (pushnew dependency (%direct-dependencies thing)))))))

(defmethod deploy ((thing job))
  (let+ (((&flet format-description ()
            (with-output-to-string (stream)
              (format stream "~@[~A~&~]~:[«no description»~:;~:*~A~]~@[~&~A~]"
                      (value thing :description.header nil) ; TODO header is a hack
                      (value thing :description nil)
                      (value thing :description.footer nil)))))
         (id   (substitute-if-not
                #\_ #'jenkins.api:job-name-character?
                (as (value thing :build-job-name) 'string)))
         (kind (let+ (((kind &optional plugin)
                       (ensure-list (value thing :kind))))
                 (if plugin
                     (list kind plugin)
                     (make-keyword (string-upcase kind)))))
         (job  (jenkins.dsl:job (kind id :description (format-description)))))
    (push job (%implementations thing))

    ;; Apply aspects, respecting declared ordering, and sort generated
    ;; builders according to declared ordering.
    (extend! job (aspects thing) thing)

    ;; TODO temp
    (xloc:->xml job (stp:root (jenkins.api::%data job)) 'jenkins.api:job)

    (let* ((existing-job  (when (jenkins.api:job? (id job))
                           (jenkins.api:job (id job))))
           (existing-kind (when existing-job
                            (jenkins.api:kind existing-job))))
      (cond
        ((not existing-job)
         (log:info "~@<Creating new job ~A~@:>" job)
         (jenkins.api::make-job (id job) (jenkins.api::%data job)))
        ((or (equal kind existing-kind)
             (case kind
               (:project (string= existing-kind "project"))
               (:matrix  (string= existing-kind "matrix-project"))))
         (log:info "~@<Updating existing job ~A~@:>" existing-job)
         (setf (jenkins.api:job-config (id job)) (jenkins.api::%data job)))
        (t
         (log:warn "~@<Deleting job ~A to change kind ~A -> ~(~A~)~@:>"
                   job (jenkins.api:kind existing-job) kind)
         (jenkins.api:delete-job (id job))
         (jenkins.api::make-job (id job) (jenkins.api::%data job)))))

    thing))

(defmethod deploy-dependencies ((thing job))
  (let ((relevant-dependencies
         (unless (as (value thing :no-dependencies nil) 'boolean) ; TODO remove
           (eswitch ((as (value thing :dependencies.mode "direct") 'string)
                     :test #'equal)
             ("direct"
              (direct-dependencies thing))
             ("minimal"
              (minimal-dependencies thing))
             ("none"
              '())))))
    (iter (for upstream-job in relevant-dependencies)
          (with-simple-restart (continue "~@<Do not relate ~A -> ~A~@:>"
                                         upstream-job thing)
            (handler-bind
                ((error (lambda (condition)
                          (error "~@<Could not relate ~A -> ~A: ~A.~@:>"
                                 upstream-job thing condition))))
              (jenkins.api:relate (implementation upstream-job) (implementation thing)
                                  :if-related nil))))))
