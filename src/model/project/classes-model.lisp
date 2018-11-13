;;;; classes-model.lisp --- Classes modeling projects, versions and jobs.
;;;;
;;;; Copyright (C) 2012-2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

;;; The object-aggregation hierarchy is as follows:
;;;
;;; project
;;;   version
;;;     job
;;;       aspect

(cl:in-package #:jenkins.model.project)

;;; `distribution' class

(defclass distribution (named-mixin
                        implementation-mixin)
  ((versions :initarg :versions
             :type    list #|of version|#
             :reader  versions))
  (:documentation
   "Instances represent implementations of `distributions-spec's.

    Contained versions are `version' instance implementing
    `version-spec's."))

(labels ((distribution-jobs (distribution)
           (remove-if-not (lambda (job)
                            (value/cast job :build-job.orchestrate? t))
                          (mappend #'jobs (versions distribution))))
         (job-name (job)
           (when-let ((job (implementation job)))
             (list (jenkins.api:id job))))
         (return-value (name value)
           (values (cons name (value-parse value)) '() t)))

  (defmethod lookup ((thing distribution) (name (eql :jobs.list))
                     &key if-undefined)
    (declare (ignore if-undefined))
    (return-value name (mapcan #'job-name (distribution-jobs thing))))

  (defmethod lookup ((thing distribution) (name (eql :jobs.dependencies))
                     &key if-undefined)
    (declare (ignore if-undefined))
    (let+ ((jobs (distribution-jobs thing))
           ((&flet dependencies (job)
              (let* ((dependency-name (value/cast thing :dependency-job-name
                                                  (name job))))
                (mappend (lambda (dependency)
                           (when-let ((dependency-job
                                       (find dependency-name (jobs dependency)
                                             :test #'string= :key #'name)))
                             (list dependency-job)))
                         (direct-dependencies (parent job))))))
           (value
            (loop :for job :in jobs
                  :collect (cons (first (job-name job))
                                 (mapcan #'job-name (dependencies job))))))
      (return-value name value)))

  (defmethod lookup ((thing distribution) (name (eql :jobs.dependencies/groovy))
                     &key if-undefined)
    (declare (ignore if-undefined))
    (return-value name (format nil "[~%~
                                      ~:{~2@T~S: [~%~
                                        ~@{~4@T~S,~%~}~
                                      ~2T],~%~}~
                                    ]"
                               (value thing :jobs.dependencies))))

  (defmethod lookup ((thing distribution) (name t)
                     &key if-undefined)
    (declare (ignore if-undefined))
    (if-let ((strategy (variable-aggregation name)))
      (let+ (((&values raw raw/next-values) ; TODO duplicates much of `value'
              (lookup (specification thing) name :if-undefined '())
              #+no (call-next-method thing name :if-undefined '()))
             ((&labels+ make-lookup ((&optional first-value &rest next-values))
                (lambda (name1 &optional (default nil default-supplied?))
                  (cond
                    ((not (eq name1 :next-value))
                     (if default-supplied?
                         (value thing name1 default)
                         (value thing name1)))
                    (first-value
                     (jenkins.model.variables::with-augmented-trace (name1 nil first-value)
                       (expand (cdr first-value) (make-lookup next-values))))
                    (default-supplied?
                     (jenkins.model.variables::with-augmented-trace (name1 :default (cons :unused default))
                       (if (functionp default) (funcall default) default)))
                    (t
                     (error "~@<No next value for ~A.~@:>"
                            name))))))
             (value           (jenkins.model.variables::with-augmented-trace (name thing raw)
                                (jenkins.model.variables::with-expansion-stack (name thing)
                                  (expand (cdr raw) (make-lookup raw/next-values)))))
             (children        (versions thing))
             (effective-value (aggregate-values value children name strategy)))
        (return-value name effective-value))
      (lookup (specification thing) name :if-undefined '()) #+no (call-next-method)))

  (defmethod lookup ((thing distribution) (name (eql :licenses))
                     &key if-undefined)
    (declare (ignore if-undefined))
    (let ((counts (make-hash-table :test #'eq)))
      (map nil (lambda (version)
                 (map nil (lambda (license)
                            (incf (gethash (make-keyword license) counts 0)))
                      (or (value version :licenses nil)
                          (ensure-list (value version :license nil))
                          (ensure-list (value version :analysis.license nil))
                          (list nil))))
           (versions thing))
      (return-value name (hash-table-alist counts)))))

(defmethod platform-requires ((object distribution) (platform t))
  (remove-duplicates
   (append (call-next-method)
           (mappend (rcurry #'platform-requires platform)
                    (versions object)))
   :test #'string=))

(defmethod check-access ((object distribution) (lower-bound t))
  (and (call-next-method)
       (if-let ((offenders (remove-if (rcurry #'check-access (access object))
                                      (versions object))))
         (let ((reasons (mapcar (lambda (version)
                                  (let+ (((&values access? reason)
                                          (check-access version (access object))))
                                    (unless access?
                                      (list version reason))))
                                offenders)))
           (values nil (make-condition
                        'simple-error
                        :format-control   "~@<~A declares ~A access but ~
                                           uses the following projects ~
                                           which do not: ~:@_~
                                           ~{~{* ~A~@[: ~A~]~}~^~:@_~}.~@:>"
                        :format-arguments (list object (access object)
                                                reasons))))
         t)))

(defmethod persons-in-role ((role t) (container distribution))
  (persons-in-role role (specification container)))

;;; `version' class

(defclass version (named-mixin
                   implementation-mixin
                   parented-mixin
                   direct-variables-mixin)
  ((direct-dependencies :initarg  :direct-dependencies
                        :type     list #| of (version . reasons) |#
                        :accessor %direct-dependencies
                        :initform '()
                        :documentation
                        "List of pairs of the form

                           (VERSION . REASONS)

                         where VERSION is either

                         + `nil' indicating unresolved requirements

                         + `:system' indicating requirements resolved
                           via a system dependency.

                         + or a `version' instance representing
                           project versions on which the project
                           version depends.

                         REASONS is a list of requirements of the form

                           (NATURE TARGET [VERSION])

                         .")
   (jobs                :initarg  :jobs
                        :type     list
                        :reader   jobs
                        :documentation
                        ""))
  (:documentation
   "Instances of this class represent versions of `project's."))

(defmethod print-items:print-items append ((object version))
  (let ((ancestor-names (list (name (parent object))
                              (name (parent (specification object)))
                              (name object))))
    `((:name ,ancestor-names "~{~A~^:~}"))))

(defmethod direct-variables ((thing version))
  (value-acons :version-name (name thing)
               (when (next-method-p)
                 (call-next-method))))

(defmethod lookup ((thing version) (name t) &key if-undefined)
  (declare (ignore if-undefined))
  (multiple-value-call #'merge-lookup-values
    (lookup (specification thing) name :if-undefined nil)
    (if (variable-inheritable? name)
        (call-next-method)
        (values nil nil nil))))

(defmethod variables append ((thing version))
  (variables (specification thing)))

(defmethod check-access ((object version) (lower-bound t))
  (let ((offender (or (when (value/cast object :scm.credentials nil)
                        :scm.credentials)
                      (when (value/cast object :scm.password nil)
                        :scm.password))))
    (cond ((not offender)
           (call-next-method))
          ((eq (access object) :public)
           (values nil (make-condition
                        'simple-error
                        :format-control "~@<Project ~A has a ~S entry but ~
                                         ~A access.~@:>"
                        :format-arguments (list object offender
                                                (access object)))))
          (t
           (call-next-method)))))

(defmethod direct-dependencies/reasons ((thing version))
  (%direct-dependencies thing))

(defmethod direct-dependencies ((thing version))
  (mapcan (lambda+ ((target . &ign))
            (unless (member target '(nil :system) :test #'eq)
              (list target)))
          (%direct-dependencies thing)))

(defmethod add-dependencies! ((thing version) (spec version-spec)
                              &key
                              (providers (missing-required-argument :providers)))
  (let+ (platform-provides (platform-provides? nil)
         ((&flet platform-provides ()
            (if platform-provides?
                platform-provides
                (setf platform-provides? t
                      platform-provides  (platform-provides thing)))))
         ((&flet add-dependency (required provider)
            (let ((cell (or (assoc provider (%direct-dependencies thing))
                            (let ((new (cons provider '())))
                              (push new (%direct-dependencies thing))
                              new))))
              (pushnew required (cdr cell) :test #'equal)))))
    (iter (for requires in (requires spec))
          (log:trace "~@<Trying to satisfy requirement ~S for ~A.~@:>"
                     requires thing)
          (restart-case
              (cond ((when-let ((match (find-provider/version
                                        requires
                                        :if-does-not-exist nil
                                        :providers         providers)))
                       (log:trace "~@<Best candidate is ~S.~@:>" match)
                       (unless (eq match thing)
                         (add-dependency requires match))
                       t))
                    ((when (find-provider/version
                            requires :providers (platform-provides))
                       (add-dependency requires :system)
                       t)))
            (continue (&optional condition)
              :report (lambda (stream)
                        (format stream "~@<Skip requirement ~S.~@:>" requires))
              (declare (ignore condition))
              ;; Record unresolved requirement.
              (add-dependency requires nil)))))

  (mapc #'add-dependencies! (jobs thing) (jobs spec)))

(defmethod deploy ((thing version))
  (mapcar #'deploy (jobs thing)))

;;; `job' class

(defclass job (named-mixin
               implementation-mixin
               specification-mixin ; TODO define another class for this
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
                        "List of aspects associated to the job."))
  (:documentation
   "Instances of this class represent build jobs which are associated
    to specific `version's of `project's."))

(defmethod lookup ((thing job) (name t) &key if-undefined)
  (declare (ignore if-undefined))
  (multiple-value-call #'merge-lookup-values
    (lookup (specification thing) name :if-undefined nil)
    (call-next-method)))

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
         (kind      (let+ (((kind &optional plugin)
                            (ensure-list (value thing :kind))))
                      (if plugin
                          (list kind plugin)
                          (make-keyword (string-upcase kind)))))
         (disabled? (value/cast thing :build-job.disabled? nil))
         ((&flet make-new-job (&optional existing-job)
            (let ((job (jenkins.dsl:job (kind id))))
              ;; Retain value of disabled slot unless
              ;; `:force-disabled' has been specified.
              (cond
                ((eq disabled? :force-disabled)
                 (setf (disabled? job) t))
                ((not existing-job)
                 (setf (disabled? job) disabled?))
                (t
                 (setf (disabled? job) (disabled? existing-job))))

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
