;;;; spec-classes.lisp ---
;;;;
;;;; Copyright (C) 2012-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

;;; The object-aggregation hierarchies are as follows:
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

#+TODO (defmethod platform-requires ((object distribution-spec) (platform t))
  (remove-duplicates
   (append (call-next-method)
           (mappend (rcurry #'platform-requires platform)
                    (versions object)))
   :test #'string=))

#+TODO (defmethod check-access ((object distribution-spec) (lower-bound t))
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

#+later (flet ((distribution-jobs (distribution)
         (let ((versions (remove nil (mapcar #'implementation
                                             (versions distribution)))))
           (remove-if-not (lambda (job)
                            (value/cast job :build-job.orchestrate? t))
                          (mappend #'jobs versions))))
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
                (mapcar (lambda (dependency)
                          (find dependency-name (jobs dependency)
                                :test #'string= :key #'name))
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
                               (value thing :jobs.dependencies)))))

#+TODO (defmethod check-access ((object version-spec) (lower-bound t))
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
