;;;; spec-classes.lisp ---
;;;;
;;;; Copyright (C) 2012-2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

;;; The object-aggregation hierarchies are as follows:
;;;
;;; distribution-spec
;;;   -> version-spec
;;;
;;; project-spec
;;;   -> template
;;;   version-spec
;;;   job-spec
;;;
;;; template
;;;   aspect-spec
;;;   job-spec

(cl:in-package #:jenkins.model.project)

(defun variable-inheritable? (name)
  (if-let ((info (find-variable name :if-does-not-exist nil)))
    (inheritance info)
    t))

(defun variable-aggregation (name)
  (when-let ((info (find-variable name :if-does-not-exist nil)))
    (aggregation info)))

;;; `distribution-spec' class

(defclass distribution-spec (named-mixin
                             direct-variables-mixin
                             person-container-mixin)
  ((versions :initarg  :versions
             :type     list ; of version-spec
             :reader   versions
             :documentation
             "Stores a list of project version specifications."))
  (:documentation
   "Instances represent specifications of distributions.

    Basically consists of variables and a set of project version
    specifications."))

(defmethod direct-variables ((thing distribution-spec))
  (value-acons :distribution-name (name thing)
               (when (next-method-p)
                 (call-next-method))))

(defmethod platform-requires ((object distribution-spec) (platform t))
  (remove-duplicates
   (append (call-next-method)
           (mappend (rcurry #'platform-requires platform)
                    (versions object)))
   :test #'string=))

(defmethod check-access ((object distribution-spec) (lower-bound t))
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

(labels ((distribution-versions (distribution)
           (remove nil (mapcar #'implementation (versions distribution))))
         (distribution-jobs (distribution)
           (remove-if-not (lambda (job)
                            (value/cast job :build-job.orchestrate? t))
                          (mappend #'jobs (distribution-versions distribution))))
         (job-name (job)
           (when-let ((job (implementation job)))
             (list (jenkins.api:id job))))
         (return-value (name value)
           (values (cons name (value-parse value)) '() t)))

  (defmethod lookup ((thing distribution-spec) (name (eql :jobs.list))
                     &key if-undefined)
    (declare (ignore if-undefined))
    (return-value name (mapcan #'job-name (distribution-jobs thing))))

  (defmethod lookup ((thing distribution-spec) (name (eql :jobs.dependencies))
                     &key if-undefined)
    (declare (ignore if-undefined))
    (let+ ((jobs (distribution-jobs thing))
           ((&flet dependencies (job)
              (let* ((dependency-name (as (value thing :dependency-job-name
                                                 (name job))
                                          'string)))
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

  (defmethod lookup ((thing distribution-spec) (name (eql :jobs.dependencies/groovy))
                     &key if-undefined)
    (declare (ignore if-undefined))
    (return-value name (format nil "[~%~
                                      ~:{~2@T~S: [~%~
                                        ~@{~4@T~S,~%~}~
                                      ~2T],~%~}~
                                    ]"
                               (value thing :jobs.dependencies))))

  (defmethod lookup ((thing distribution-spec) (name t)
                     &key if-undefined)
    (declare (ignore if-undefined))
    (if-let ((strategy        (variable-aggregation name)))
      (let+ (((&values raw raw/next-values) ; TODO duplicates much of `value'
              (call-next-method thing name :if-undefined '()))
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
             (children        (distribution-versions thing))
             (effective-value (aggregate-values value children name strategy)))
        (return-value name effective-value))
      (call-next-method)))

  (defmethod lookup ((thing distribution-spec) (name (eql :licenses))
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
           (distribution-versions thing))
      (return-value name (hash-table-alist counts)))))

;;; `project-spec' class

(defclass project-spec (named-mixin
                        specification-mixin
                        direct-variables-mixin
                        parented-mixin)
  ((templates :initarg  :templates
              :type     list ; of template
              :reader   templates
              :initform '()
              :documentation
              "")
   (versions  :initarg  :versions
              :type     list ; of version-spec
              :accessor versions
              :initform '()
              :documentation
              ""))
  (:documentation
   "Instances of this class describe projects.

    `project-spec' instances can reference zero or more `template'
    instances from which variables, version specifications and job
    specifications are inherited.

    In addition, `project-spec' instances directly contain version
    specifications."))

(defmethod direct-variables ((thing project-spec))
  (value-acons :project-name (name thing)
               (when (next-method-p)
                 (call-next-method))))

(defmethod variables :around ((thing project-spec))
  (append ;; TODO(jmoringe, 2013-02-22): this is a hack to add our
          ;; direct variables in front of variables from
          ;; templates. maybe variables should not have `append'
          ;; method combination?
          (direct-variables thing)
          (mappend #'variables (templates thing))
          ;; TODO this is a hack to not inherit the values of certain
          ;; variables from parents like `distribution-spec'
          ;; instances.
          (when-let ((parent (parent thing)))
            (remove-if-not #'variable-inheritable? (variables parent)
                           :key #'car))))

(defmethod lookup ((thing project-spec) (name t) &key if-undefined)
  (declare (ignore if-undefined))
  ;; The next method is (modulo `named-mixin') the one specialized on
  ;; `direct-variables-mixin', meaning that variables defined in the
  ;; parent are not included in the initial value.
  (multiple-value-call #'merge-lookup-values
    (values-list
     (reduce #'merge-lookup-results
             (mapcar (lambda (template)
                       (multiple-value-list
                        (lookup template name :if-undefined nil)))
                     (templates thing))
             :initial-value (multiple-value-list (call-next-method))))
    (if (and (parent thing) (variable-inheritable? name))
        (lookup (parent thing) name :if-undefined nil)
        (values nil '() nil))))

(defmethod aspects ((thing project-spec))
  (remove-duplicates (mappend #'aspects (templates thing))
                     :test     #'string=
                     :key      #'name
                     :from-end t))

(defmethod jobs ((thing project-spec))
  (remove-duplicates (mappend #'jobs (templates thing))
                     :test     #'string=
                     :key      #'name
                     :from-end t))

(defmethod instantiate ((spec project-spec) &key parent specification-parent)
  (declare (ignore parent specification-parent))
  (let+ (((&flet make-version (spec parent)
            (when (instantiate? spec parent)
              (when-let ((version (instantiate spec :parent parent)))
                (list version)))))
         (project (make-instance 'project
                                 :name          (name spec)
                                 :variables     '()
                                 :specification spec)))
    (reinitialize-instance
     project
     :versions (mapcan (rcurry #'make-version project) (versions spec)))))

;;; `version-spec' class

(defclass version-spec (named-mixin
                        specification-mixin
                        conditional-mixin
                        parented-mixin
                        direct-variables-mixin
                        person-container-mixin)
  ((requires :initarg  :requires
             :type     list
             :accessor %requires
             :initform '()
             :documentation
             "A list of requirement descriptions. Elements are of the
              form

                (NATURE NAME [VERSION])

             .")
   (provides :initarg  :provides
             :type     list
             :reader   %provides
             :initform '()
             :documentation
             "A list of descriptions of provided things. Elements are
              of the form

                (NATURE NAME [VERSION])

              ."))
  (:documentation
   "Instances are project version specifications.

    Consists of variables, additional requirements and provided things
    and instantiation conditions."))

(defmethod shared-initialize :after ((instance   version-spec)
                                     (slot-names t)
                                     &key)
  ;; TODO moved from dump.lisp; but is not the right place, either
  (iter (for (mechanism name . version) in (provides instance))
        (push-provider
         instance
         (list* mechanism name
                (when (first version) (list (first version)))))))

(defmethod direct-variables ((thing version-spec))
  (value-acons :version-name (name thing)
               (when (next-method-p)
                 (call-next-method))))

(defmethod aspects ((thing version-spec))
  (aspects (parent thing)))

(defmethod jobs ((thing version-spec))
  (jobs (parent thing)))

(defmethod requires :around ((spec version-spec))
  (jenkins.analysis:merge-dependencies (call-next-method)))

(defmethod provides :around ((spec version-spec))
  (jenkins.analysis:merge-dependencies (call-next-method)))

(defmethod requires ((spec version-spec))
  (append (mapcar #'parse-dependency-spec
                  (value/cast spec :extra-requires '()))
          (%requires spec)))

(defmethod provides ((spec version-spec))
  (append (mapcar #'parse-dependency-spec
                  (value/cast spec :extra-provides '()))
          (%provides spec)))

(defmethod requires-of-kind ((nature t) (spec version-spec))
  (remove nature (requires spec)
          :test (complement #'eq)
          :key  #'first))

(defmethod provides-of-kind ((nature t) (spec version-spec))
  (remove nature (provides spec)
          :test (complement #'eq)
          :key  #'first))

(defmethod check-access ((object version-spec) (lower-bound t))
  (let ((offender (or (when (value/cast object :scm.credentials nil)
                        :scm.credentials)
                      (when (value/cast object :scm.password nil)
                        :scm.password))))
    (cond
      ((not offender)
       (call-next-method))
      ((eq (access object) :public)
       (values nil (make-condition
                    'simple-error
                    :format-control   "~@<Project ~A has a ~S entry but ~
                                       ~A access.~@:>"
                    :format-arguments (list object offender
                                            (access object)))))
      (t
       (call-next-method)))))

(defmethod instantiate ((spec version-spec) &key parent specification-parent)
  (declare (ignore specification-parent))
  (let+ (((&flet make-job (job-spec parent)
            (when (instantiate? job-spec parent)
              (when-let ((job (instantiate job-spec
                                           :parent               parent
                                           :specification-parent spec)))
                (list job)))))
         (version (make-instance 'version
                                 :name      (name spec)
                                 :parent    parent
                                 :variables (direct-variables spec))))
    (reinitialize-instance
     version
     :jobs (mapcan (rcurry #'make-job version) (jobs spec)))))

;;; `job-spec' class

(defclass job-spec (named-mixin
                    specification-mixin
                    conditional-mixin
                    parented-mixin
                    direct-variables-mixin)
  ()
  (:documentation
   "Specification of a build job to be generated."))

(defmethod instantiate ((spec job-spec) &key parent specification-parent)
  (let+ (((&flet make-aspect (spec parent)
            (when (instantiate? spec parent)
              (when-let ((aspect (instantiate spec :parent parent)))
                (list aspect)))))
         (job (make-instance 'job
                             :name      (name spec)
                             :parent    parent
                             :variables (copy-list (direct-variables spec))))) ; TODO copy-list?
    (reinitialize-instance
     job :aspects (mapcan (rcurry #'make-aspect job)
                          (aspects specification-parent)))))

;;; `template' class

(defclass template (named-mixin
                    direct-variables-mixin)
  ((inherit :initarg  :inherit
            :type     list
            :reader   inherit
            :initform '()
            :documentation
            "")
   (aspects :initarg  :aspects
            :type     list ; of aspect
            :reader   direct-aspects
            :initform '()
            :documentation
            "")
   (jobs    :initarg  :jobs
            :type     list ; of job
            :reader   direct-jobs
            :initform '()
            :documentation
            ""))
  (:documentation
   "A collection of variables, aspects and jobs to use for a project version."))

;;; TODO(jmoringe, 2013-01-16): move to correct file

(defmethod variables :around ((thing template))
  (append (call-next-method) (mappend #'variables (inherit thing))))

(defmethod lookup ((thing template) (name t) &key if-undefined)
  (declare (ignore if-undefined))
  (values-list
   (reduce #'merge-lookup-results
           (mapcar (lambda (inherited)
                     (multiple-value-list
                      (lookup inherited name :if-undefined nil)))
                   (inherit thing))
           :initial-value (multiple-value-list (call-next-method)))))

(defmethod aspects ((thing template))
  (remove-duplicates (append (direct-aspects thing)
                             (mappend #'aspects (inherit thing)))
                     :test     #'string=
                     :key      #'name
                     :from-end t))

(defmethod jobs ((thing template))
  (remove-duplicates (append (direct-jobs thing)
                             (mappend #'jobs (inherit thing)))
                     :test     #'string=
                     :key      #'name
                     :from-end t))

;;; `aspect-spec'

(defclass aspect-spec (named-mixin
                       specification-mixin
                       conditional-mixin
                       parented-mixin
                       direct-variables-mixin)
  ((aspect :initarg  :aspect
           :type     string
           :reader   aspect
           :documentation
           "Name of the aspect class that should be used to implement
            this specification."))
  (:default-initargs
   :aspect (missing-required-initarg 'aspect-spec :aspect))
  (:documentation
   "Specification of an aspect that should be applied to a build job."))

(defmethod instantiate ((spec aspect-spec) &key parent specification-parent)
  (declare (ignore specification-parent))
  (make-aspect (aspect spec)
               :name      (name spec)
               :parent    parent
               :variables (direct-variables spec)))

;;; `person'

(defclass person (rosetta-project.model.resource:person
                  direct-variables-mixin)
  ()
  (:documentation
   "Adds variables to the rosetta-project `person' class."))

(defmethod rosetta-project.model.resource:augment-person!
    ((person rosetta-project.model.resource:person) (other-person person))
  (rosetta-project.model.resource:augment-person!
   (change-class person 'person :variables (direct-variables other-person))
   other-person))
