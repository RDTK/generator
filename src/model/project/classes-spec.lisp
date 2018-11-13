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
                             person-container-mixin
                             specification-mixin)
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

(defmethod instantiate ((spec distribution-spec) &key parent specification-parent)
  (declare (ignore parent specification-parent))
  (let+ ((version-specs (versions spec))
         (distribution  (make-instance 'distribution
                                       :name          (name spec)
                                       :specification spec))
         ((&flet make-version (version-spec)
            (when-let  ((version (instantiate version-spec :parent distribution)))
              (list version))))
         (versions  (mapcan #'make-version version-specs))
         (providers (make-hash-table :test #'equal)))
    ;; Build a table of provided things and providers.
    (map nil (lambda (version)
               (map nil (lambda (provided)
                          (push version (gethash provided providers '())))
                    (provides (specification version))))
         versions)
    ;; After all `version' instances have been made, resolve
    ;; dependencies among them.
    (let ((providers (hash-table-alist providers)))
      (map nil (lambda (version)
                 (add-dependencies! version (specification version)
                                    :providers providers))
           versions))
    (reinitialize-instance distribution :versions versions)))

;;; `project-spec' class

(defclass project-spec (named-mixin
                        specification-mixin
                        direct-variables-mixin)
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
          (mappend #'variables (templates thing))))

(defmethod lookup ((thing project-spec) (name t) &key if-undefined)
  (declare (ignore if-undefined))
  ;; The next method is (modulo `named-mixin') the one specialized on
  ;; `direct-variables-mixin', meaning that variables defined in the
  ;; parent are not included in the initial value.
  (values-list
   (reduce #'merge-lookup-results
           (mapcar (lambda (template)
                     (multiple-value-list
                      (lookup template name :if-undefined nil)))
                   (templates thing))
           :initial-value (multiple-value-list (call-next-method)))))

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

;;; `version-spec' class

(defclass version-spec (named-mixin
                        specification-mixin
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

(defmethod instantiate ((spec version-spec) &key parent specification-parent)
  (declare (ignore specification-parent))
  (let+ (((&flet make-job (job-spec parent)
            (when (instantiate? job-spec parent)
              (when-let ((job (instantiate job-spec
                                           :parent               parent
                                           :specification-parent spec)))
                (list job)))))
         (version (make-instance 'version
                                 :name          (name spec)
                                 :specification spec
                                 :parent        parent
                                 :variables     (direct-variables spec))))
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

(defmethod direct-variables ((thing job-spec))
  (value-acons :job-name (name thing)
               (when (next-method-p)
                 (call-next-method))))

(defmethod instantiate ((spec job-spec) &key parent specification-parent)
  (let+ (((&flet make-aspect (spec parent)
            (when (instantiate? spec parent)
              (when-let ((aspect (instantiate spec :parent parent)))
                (list aspect)))))
         (job (make-instance 'job
                             :name          (name spec)
                             :specification spec
                             :parent        parent)))
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
