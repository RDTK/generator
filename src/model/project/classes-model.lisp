;;;; classes-model.lisp --- Classes modeling projects, versions and jobs.
;;;;
;;;; Copyright (C) 2012-2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

;;; The object-aggregation hierarchy is as follows:
;;;
;;; project
;;;   version
;;;     job
;;;       aspect

(cl:in-package #:jenkins.model.project)

;;; `platform-dependency' class

(defclass platform-dependency (model:named-mixin
                               var:direct-variables-mixin
                               dependencies-from-variables-mixin)
  ()
  (:documentation
   "Instances represent dependency relations of platform packages."))

(defun make-platform-dependency (name variables)
  (make-instance 'platform-dependency
                 :name      name
                 :variables (process-variables variables)))

;;; `distribution' class

(defclass distribution (model:named-mixin
                        model:implementation-mixin)
  ((versions :initarg :versions
             :type    list #|of version|#
             :reader  versions))
  (:documentation
   "Instances represent implementations of `distributions-spec's.

    Contained versions are `version' instance implementing
    `version-spec's."))

(labels ((distribution-jobs (distribution)
           (remove-if-not (lambda (job)
                            (var:value/cast job :build-job.orchestrate? t))
                          (mappend #'jobs (versions distribution))))
         (job-name (job)
           (when-let ((job (model:implementation job)))
             (list (jenkins.api:id job))))
         (return-value (name value)
           (values (cons name (var:value-parse value)) '() t)))

  (defmethod var:lookup ((thing distribution) (name (eql :jobs.list))
                         &key if-undefined)
    (declare (ignore if-undefined))
    (return-value name (mapcan #'job-name (distribution-jobs thing))))

  (defmethod var:lookup ((thing distribution) (name (eql :jobs.dependencies))
                         &key if-undefined)
    (declare (ignore if-undefined))
    (let+ ((jobs (distribution-jobs thing))
           ((&flet dependencies (job)
              (let* ((dependency-name (var:value/cast thing :dependency-job-name
                                                      (model:name job))))
                (mappend (lambda (dependency)
                           (when-let ((dependency-job
                                       (find dependency-name (jobs dependency)
                                             :test #'string= :key #'model:name)))
                             (list dependency-job)))
                         (model:direct-dependencies (model:parent job))))))
           (value
            (loop :for job :in jobs
                  :collect (cons (first (job-name job))
                                 (mapcan #'job-name (dependencies job))))))
      (return-value name value)))

  (defmethod var:lookup ((thing distribution) (name (eql :jobs.dependencies/groovy))
                         &key if-undefined)
    (declare (ignore if-undefined))
    (return-value name (format nil "[~%~
                                      ~:{~2@T~S: [~%~
                                        ~@{~4@T~S,~%~}~
                                      ~2T],~%~}~
                                    ]"
                               (var:value thing :jobs.dependencies))))

  (defmethod var:lookup ((thing distribution) (name t)
                         &key if-undefined)
    (declare (ignore if-undefined))
    (if-let ((strategy (variable-aggregation name)))
      (let+ (((&values raw raw/next-values) ; TODO duplicates much of `value'
              (var:lookup (model:specification thing) name :if-undefined '())
              #+no (call-next-method thing name :if-undefined '()))
             ((&labels+ make-lookup ((&optional first-value &rest next-values))
                (lambda (name1 &optional (default nil default-supplied?))
                  (cond
                    ((not (eq name1 :next-value))
                     (if default-supplied?
                         (var:value thing name1 default)
                         (var:value thing name1)))
                    (first-value
                     (var::with-augmented-trace (name1 nil first-value)
                       (var:expand (cdr first-value) (make-lookup next-values))))
                    (default-supplied?
                     (var::with-augmented-trace (name1 :default (cons :unused default))
                       (if (functionp default) (funcall default) default)))
                    (t
                     (error "~@<No next value for ~A.~@:>"
                            name))))))
             (value           (var::with-augmented-trace (name thing raw)
                                (var::with-expansion-stack (name thing)
                                  (var:expand (cdr raw) (make-lookup raw/next-values)))))
             (children        (versions thing))
             (effective-value (var:aggregate-values value children name strategy)))
        (return-value name effective-value))
      (var:lookup (model:specification thing) name :if-undefined '()) #+no (call-next-method)))

  (defmethod var:lookup ((thing distribution) (name (eql :licenses))
                         &key if-undefined)
    (declare (ignore if-undefined))
    (let ((counts (make-hash-table :test #'eq)))
      (map nil (lambda (version)
                 (map nil (lambda (license)
                            (incf (gethash (make-keyword license) counts 0)))
                      (or (var:value version :licenses nil)
                          (ensure-list (var:value version :license nil))
                          (ensure-list (var:value version :analysis.license nil))
                          (list nil))))
           (versions thing))
      (return-value name (hash-table-alist counts)))))

(defmethod platform-requires ((object distribution) (platform t))
  (append (call-next-method)
          (mappend (rcurry #'platform-requires platform)
                   (versions object))))

(defmethod model:check-access ((object distribution) (lower-bound t))
  (and (call-next-method)
       (if-let ((offenders (remove-if (rcurry #'model:check-access (model:access object))
                                      (versions object))))
         (let ((value   (cdr (var:lookup object :access :if-undefined nil)))
               (reasons (mapcar (lambda (version)
                                  (let+ (((&values access? reason)
                                          (model:check-access version (model:access object))))
                                    (unless access?
                                      (list (print-items:print-items version)
                                            reason))))
                                offenders)))
           (values nil (make-object-error
                        (when value
                          (list (list value "distribution access declaration" :info)))
                        "~@<~A declares ~A access but uses the ~
                         following projects which do not:~:@_~
                         ~{• ~<~@;~
                           ~/print-items:format-print-items/~@[:~@:_~
                           ~2@T~A~]~
                         ~:>~^~:@_~}~@:>"
                        object (model:access object) reasons)))
         t)))

(defmethod persons-in-role ((role t) (container distribution))
  (persons-in-role role (model:specification container)))

;;; `include-context' class

(defclass include-context (var:direct-variables-mixin)
  ((distribution :initarg :distribution
                 :reader  distribution)))

;;; `version' class

(defclass version (model:named-mixin
                   model:implementation-mixin
                   model:parented-mixin
                   var:direct-variables-mixin
                   dependencies-from-variables-mixin)
  ((context               :initarg  :context
                          :type     (or null include-context)
                          :reader   context
                          :initform nil
                          :documentation
                          "Stores the `include-context' instance for the version.

                           This context stores the distribution in which
                           this project version was originally included
                           as well parameters specified at the point of
                           inclusion.")
   (direct-dependencies   :initarg  :direct-dependencies
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
   (platform-dependencies :initarg  :direct-platform-dependencies
                          :type     list #| of (platform-dependency . reasons) |#
                          :reader   direct-platform-dependencies/reasons
                          :accessor %direct-platform-dependencies
                          :initform '()
                          :documentation
                          "List of `platform-dependency' instances
                           representing platform dependencies of the
                           project version.")
   (jobs                  :initarg  :jobs
                          :type     list
                          :reader   jobs
                          :documentation
                          ""))
  (:documentation
   "Instances of this class represent versions of `project's."))

(defmethod model:ancestor-names ((thing version))
  (list (model:name thing)
        (model:name (model:parent (model:specification thing)))
        (model:name (model:parent thing))))

(defmethod var:direct-variables ((thing version))
  (var:value-acons :version-name (model:name thing)
                   (when (next-method-p)
                     (call-next-method))))

(defmethod var:lookup ((thing version) (name t) &key if-undefined)
  (declare (ignore if-undefined))
  (let ((inheritable? (variable-inheritable? name))
        (context      (context thing)))
   (values-list
    (reduce #'var:merge-lookup-results
            (list (multiple-value-list
                   (var:lookup (model:specification thing) name :if-undefined nil))

                  ;; Parameters specified at point of inclusion.
                  (if context
                      (multiple-value-list
                       (var:lookup context name :if-undefined nil))
                      '(nil nil nil))

                  ;; Variables in the distribution in which this
                  ;; project version was originally included.
                  ;;
                  ;; `inheritable?' can be `:outermost-only' in which
                  ;; case we don't look in the context distribution.
                  (if (and (eq inheritable? t) context (distribution context))
                      (multiple-value-list
                       (var:lookup (distribution context) name :if-undefined nil))
                      '(nil nil nil))

                  (if inheritable?
                      (when-let ((parent (model:parent thing)))
                        (multiple-value-list
                         (var:lookup parent name :if-undefined nil)))
                      '(nil nil nil)))

            :initial-value (multiple-value-list (var:direct-lookup thing name))))))

(defmethod var:variables append ((thing version))
  (var:variables (model:specification thing)))

(defmethod model:check-access ((object version) (lower-bound t))
  (let+ (((&flet check-variable (name)
            (when (var:value/cast object name nil)
              (list name (cdr (var:lookup object name))))))
         ((&optional offender value)
          (or (check-variable :scm.credentials)
              (check-variable :scm.password))))
    (cond ((not offender)
           (call-next-method))
          ((eq (model:access object) :public)
           (values nil (make-object-error
                        (list (list value "credential declaration" :error))
                        "~@<Project ~A has a ~S entry but ~A access.~@:>"
                        object offender (model:access object))))
          (t
           (call-next-method)))))

(defmethod requires ((spec version))
  (append (call-next-method)
          (direct-requires (model:specification spec))))

(defmethod provides ((spec version))
  (append (call-next-method)
          (direct-provides (model:specification spec))))

(defmethod direct-dependencies/reasons ((thing version))
  (%direct-dependencies thing))

(defmethod model:direct-dependencies ((thing version))
  (mapcan (lambda+ ((target . &ign))
            (unless (member target '(nil :system) :test #'eq)
              (list target)))
          (%direct-dependencies thing)))

(defmethod direct-platform-dependencies ((thing version))
  (map 'list #'car (%direct-platform-dependencies thing)))

(defun index-platform-provides (thing)
  (with-simple-restart (continue "~@<Do not compute platform provides.~@:>")
    (reduce (lambda+ (index (dependency . provider))
              (jenkins.analysis:index-provider! dependency provider index))
            (platform-provides thing)
            :initial-value (jenkins.analysis:make-provider-index))))

(defmethod model:add-dependencies! ((thing version)
                                    &key
                                    (providers (missing-required-argument :providers)))
  (let+ (platform-provides (platform-provides? nil)
         ((&flet platform-provides ()
            (if platform-provides?
                platform-provides
                (setf platform-provides? t
                      platform-provides  (index-platform-provides thing)))))
         ((&flet add-dependency (required provider)
            (let ((cell (or (assoc provider (%direct-dependencies thing))
                            (let ((new (cons provider '())))
                              (push new (%direct-dependencies thing))
                              new))))
              (pushnew required (cdr cell) :test #'equal))))
         ((&flet add-platform-dependency (required provider)
            (let ((cell (or (assoc provider (%direct-platform-dependencies thing))
                            (let ((new (cons provider '())))
                              (push new (%direct-platform-dependencies thing))
                              new))))
              (pushnew required (cdr cell) :test #'equal)))))
    (iter (for requires in (requires thing))
          (log:trace "~@<Trying to satisfy requirement ~S for ~A.~@:>"
                     requires thing)
          (restart-case
              (cond ;; Search in distribution-provided features.
                    ((when-let ((match (find-provider/version
                                        requires providers
                                        :if-does-not-exist nil)))
                       (log:trace "~@<Best candidate is ~S.~@:>" match)
                       (unless (eq match thing)
                         (add-dependency requires match))
                       t))
                    ;; Search in platform-provided features.
                    ((when-let* ((platform-provides (platform-provides))
                                 (provider          (find-provider/version
                                                     requires platform-provides)))
                       (typecase provider
                         (platform-dependency
                          (add-platform-dependency requires provider))
                         (t
                          (add-dependency requires :system)))
                       t)))
            (continue (&optional condition)
              :report (lambda (stream)
                        (format stream "~@<Skip requirement ~S.~@:>" requires))
              (declare (ignore condition))
              ;; Record unresolved requirement.
              (add-dependency requires nil)))))

  (mapc #'model:add-dependencies! (jobs thing)))

(defmethod platform-requires ((object version) (platform cons))
  (append (call-next-method)
          (mappend (rcurry #'platform-requires platform)
                   (direct-platform-dependencies object))))

(defmethod model:deploy ((thing version))
  (with-sequence-progress (:deploy/job (jobs thing))
    (mapcar #'model:deploy (jobs thing))))

(defvar *outermost-version?* t)

(defmethod model:deploy :around ((thing version))
  (if *outermost-version?*
      (with-condition-translation (((error model:project-deployment-error)
                                    :thing thing))
        (let ((*outermost-version?* nil))
          (call-next-method)))
      (call-next-method)))

;;; `job' class

(defclass job (model:named-mixin
               model:implementation-mixin
               model:specification-mixin ; TODO define another class for this
               model:parented-mixin)
  ((direct-dependencies :initarg  :direct-dependencies ; TODO(jmoringe, 2013-03-06): dependencies-mixin?
                        :type     list ; of job
                        :reader   model:direct-dependencies
                        :accessor %direct-dependencies
                        :initform '()
                        :documentation
                        "List of other `job' instances.")
   (aspects             :initarg  :aspects
                        :type     list
                        :reader   aspects:aspects
                        :initform '()
                        :documentation
                        "List of aspects associated to the job."))
  (:documentation
   "Instances of this class represent build jobs which are associated
    to specific `version's of `project's."))

(defmethod var:lookup ((thing job) (name t) &key if-undefined)
  (declare (ignore if-undefined))
  (multiple-value-call #'var:merge-lookup-values
    (var:lookup (model:specification thing) name :if-undefined nil)
    (call-next-method)))

(defmethod model:add-dependencies! ((thing job) &key providers)
  (declare (ignore providers))
  (let ((dependency-name (var:value/cast thing :dependency-job-name (model:name thing))))
    (iter (for dependency in (model:direct-dependencies (model:parent thing)))
          (log:trace "~@<Trying to add ~A to ~A~@:>" dependency thing)
          (with-simple-restart (continue "~@<Skip adding dependency ~A.~@:>"
                                         dependency)
            (let ((dependency
                    (or (find dependency-name (jobs dependency)
                              :test #'string= :key #'model:name)
                        (error "~@<Could not find ~S in the jobs of ~
                               ~A~@[ (~{~A~^, ~})~]~@:>"
                               dependency-name dependency (jobs dependency)))))
              (pushnew dependency (%direct-dependencies thing)))))))

(defmethod model:deploy ((thing job))
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

(defmethod model:deploy-dependencies ((thing job))
  (let ((relevant-dependencies
         (ecase (var:value/cast thing :dependencies.mode :direct)
           (:direct  (model:direct-dependencies thing))
           (:minimal (model:minimal-dependencies thing))
           (:none    '()))))
    (iter (for upstream-job in relevant-dependencies)
          (with-simple-restart (continue "~@<Do not relate ~A -> ~A~@:>"
                                         upstream-job thing)
            (handler-bind
                ((error (lambda (condition)
                          (error "~@<Could not relate ~A -> ~A: ~A.~@:>"
                                 upstream-job thing condition))))
              (jenkins.api:relate (model:implementation upstream-job)
                                  (model:implementation thing)
                                  :if-related nil))))))
