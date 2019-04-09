;;;; spec-classes.lisp ---
;;;;
;;;; Copyright (C) 2012-2019, 2022 Jan Moringen
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

(cl:in-package #:build-generator.model.project)

(defun variable-inheritable? (name)
  (if-let ((info (var:find-variable name :if-does-not-exist nil)))
    (var:inheritance info)
    t))

(defun variable-aggregation (name)
  (when-let ((info (var:find-variable name :if-does-not-exist nil)))
    (var:aggregation info)))

;;; `distribution-include'

(defclass distribution-include (var:direct-variables-mixin
                                print-items:print-items-mixin)
  ((distribution :initarg :distribution
                 :type    distribution-spec
                 :reader  distribution)))

(defmethod print-items:print-items append ((object distribution-include))
  (let ((distribution (model:name (distribution object))))
    `((:distribution "~A" ,distribution))))

;;; `project-include'

(defclass project-include (var:direct-variables-mixin
                           print-items:print-items-mixin)
  ((project :initarg :project
            :type    string
            :reader  project)
   (version :initarg :version
            :type    string
            :reader  version)))

(defmethod print-items:print-items append ((object project-include))
  `((:project                     "~A"  ,(project object))
    ((:version (:after :project)) "@~A" ,(version object))))

;;; `resolved-project-include'

(defclass resolved-project-include (var:direct-variables-mixin
                                    model:implementation-mixin
                                    print-items:print-items-mixin)
  ((version :initarg :version
            :reader  version)))

(defmethod print-items:print-items append ((object resolved-project-include))
  (let ((version (print-items:print-items (version object))))
    `((:version "~/print-items:format-print-items/" ,version))))

;;; `distribution-spec' class

(defclass distribution-spec (model:named+builtin-entries-mixin
                             var:direct-variables-mixin
                             person-container-mixin
                             model:specification-mixin)
  ((direct-includes :initarg  :direct-includes
                    :type     list      ; of `distribution-include'
                    :reader   direct-includes
                    :initform '())
   (direct-versions :initarg  :direct-versions
                    :type     list      ; of `[resolved-]project-include'
                    :reader   direct-versions
                    :documentation
                    "Stores a list of project version specifications."))
  (:documentation
   "Instances represent specifications of distributions.

    Basically consists of variables and a set of project version
    specifications."))

(defmethod model:name-variable ((thing distribution-spec))
  :distribution-name)

(defmethod versions ((object distribution-spec))
  (append (direct-versions object)
          (mappend (compose #'versions #'distribution)
                   (direct-includes object))))

(defmethod model:instantiate ((spec distribution-spec) &key parent specification-parent)
  (declare (ignore parent specification-parent))
  (let+ ((distribution (make-instance 'distribution
                                      :name          (model:name spec)
                                      :specification spec))
         (seen-versions (make-hash-table :test #'eq))
         ((&flet make-version (project-include &key context)
            (let+ (((&accessors-r/o version (parameters var:direct-variables))
                    project-include))
              (unless (gethash version seen-versions)
                (setf (gethash version seen-versions) t)
                (when-let ((version (model:instantiate version
                                                       :parent    distribution
                                                       :context   context
                                                       :variables parameters)))
                  (list version))))))
         ;; Process one included distribution, making
         ;; `include-context' instances for the direct versions. This
         ;; results in self-contained `version' instances. The include
         ;; relations between distributions are not represented in the
         ;; final `distribution' instance and its `version' instances.
         ((&labels one-distribution-include (distribution-include)
            (let+ (((&accessors-r/o distribution (parameters var:direct-variables))
                    distribution-include)
                   (context (make-instance 'include-context
                                           :distribution distribution
                                           :variables    parameters)))
              (append (mapcan (rcurry #'make-version :context context)
                              (direct-versions distribution))
                      (mappend #'one-distribution-include
                               (direct-includes distribution))))))
         ;; Process SPEC's direct versions without making include
         ;; contexts. Process SPEC's included distributions (direct
         ;; and transitive) with include contexts.
         (versions  (append (mapcan #'make-version (direct-versions spec))
                            (mappend #'one-distribution-include
                                     (direct-includes spec)))))

    ;; Build a table of provided things and providers.
    (let ((provider-index (analysis:make-provider-index)))
      (map nil (lambda (version)
                 (map nil (rcurry #'analysis:index-provider!
                                  version provider-index)
                      (provides version)))
           versions)

      ;; After all `version' instances have been made, resolve
      ;; dependencies among them.
      (map nil (rcurry #'model:add-dependencies! :providers provider-index)
           versions))

    (reinitialize-instance distribution :versions versions)))

;;; `project-spec' class

(defclass project-spec (model:named+builtin-entries-mixin
                        model:specification-mixin
                        var:direct-variables-mixin)
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

(defmethod model:name-variable ((thing project-spec))
  :project-name)

(defmethod var:variables :around ((thing project-spec))
  (append ;; TODO(jmoringe, 2013-02-22): this is a hack to add our
          ;; direct variables in front of variables from
          ;; templates. maybe variables should not have `append'
          ;; method combination?
          (var:direct-variables thing)
          (mappend #'var:variables (templates thing))))

(defmethod var:lookup ((thing project-spec) (name t) &key if-undefined)
  (declare (ignore if-undefined))
  ;; The next method is (modulo `named-mixin') the one specialized on
  ;; `direct-variables-mixin', meaning that variables defined in the
  ;; parent are not included in the initial value.
  (values-list
   (reduce #'var:merge-lookup-results
           (mapcar (lambda (template)
                     (multiple-value-list
                      (var:lookup template name :if-undefined nil)))
                   (templates thing))
           :initial-value (multiple-value-list (call-next-method)))))

(defmethod aspects:aspects ((thing project-spec))
  (remove-duplicates (mappend #'aspects:aspects (templates thing))
                     :test     #'string=
                     :key      #'model:name
                     :from-end t))

(defmethod jobs ((thing project-spec))
  (remove-duplicates (mappend #'jobs (templates thing))
                     :test     #'string=
                     :key      #'model:name
                     :from-end t))

;;; `version-spec' class

(defclass version-spec (model:specification-mixin
                        model:parented-mixin
                        model:named+builtin-entries-mixin
                        var:direct-variables-mixin
                        dependency-merging-mixin
                        dependencies-mixin
                        dependencies-from-variables-mixin
                        person-container-mixin)
  ()
  (:documentation
   "Instances are project version specifications.

    Consists of variables, additional requirements and provided things
    and instantiation conditions."))

(defmethod model:name-variable ((thing version-spec))
  :version-name)

(defvar *global-stuff*
  (make-instance 'var:direct-variables-mixin))

(defun apply-replacements (variable value defaulted?)
  (let ((rules (var:value *global-stuff* variable '())))
    (values (reduce (lambda+ (value (pattern template))
                      (ppcre:regex-replace pattern value template))
                    rules :initial-value value)
            defaulted?)))

(defmethod var:value ((thing version-spec) (name (eql :repository)) &optional default)
  (declare (ignore default))
  (multiple-value-call #'apply-replacements
    :url-replacements (call-next-method)))

(defmethod aspects:aspects ((thing version-spec))
  (aspects:aspects (model:parent thing)))

(defmethod jobs ((thing version-spec))
  (jobs (model:parent thing)))

(defmethod model:instantiate ((spec version-spec) &key parent specification-parent
                                                       context variables)
  (declare (ignore specification-parent))
  (let+ ((version (make-instance 'version
                                 :name          (model:name spec)
                                 :specification spec
                                 :parent        parent
                                 :context       context
                                 :variables     variables))
         ((&flet make-job (job-spec)
            (when (model:instantiate? job-spec version)
              (when-let ((job (model:instantiate job-spec
                                                 :parent               version
                                                 :specification-parent spec)))
                (list job))))))
    (reinitialize-instance version :jobs (mapcan #'make-job (jobs spec)))))

;;; `job-spec' class

(defclass job-spec (model:specification-mixin
                    model:conditional-mixin
                    model:parented-mixin
                    model:named+builtin-entries-mixin
                    var:direct-variables-mixin)
  ()
  (:documentation
   "Specification of a build job to be generated."))

(defmethod model:name-variable ((thing job-spec))
  :job-name)

(defmethod model:instantiate ((spec job-spec) &key parent specification-parent)
  (let+ ((job (make-instance 'job :name          (model:name spec)
                                  :specification spec
                                  :parent        parent))
         ((&flet make-aspect (spec)
            (when (model:instantiate? spec job)
              (when-let ((aspect (model:instantiate spec :parent job)))
                (list aspect))))))
    (reinitialize-instance
     job :aspects (mapcan #'make-aspect (aspects:aspects specification-parent)))))

;;; `template' class

(defclass template (model:named-mixin
                    var:direct-variables-mixin)
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

(defmethod var:variables :around ((thing template))
  (append (call-next-method) (mappend #'var:variables (inherit thing))))

(defmethod var:lookup ((thing template) (name t) &key if-undefined)
  (declare (ignore if-undefined))
  (values-list
   (reduce #'var:merge-lookup-results
           (mapcar (lambda (inherited)
                     (multiple-value-list
                      (var:lookup inherited name :if-undefined nil)))
                   (inherit thing))
           :initial-value (multiple-value-list (call-next-method)))))

(defmethod aspects:aspects ((thing template))
  (remove-duplicates (append (direct-aspects thing)
                             (mappend #'aspects:aspects (inherit thing)))
                     :test     #'string=
                     :key      #'model:name
                     :from-end t))

(defmethod jobs ((thing template))
  (remove-duplicates (append (direct-jobs thing)
                             (mappend #'jobs (inherit thing)))
                     :test     #'string=
                     :key      #'model:name
                     :from-end t))

;;; `aspect-spec'

(defclass aspect-spec (model:named-mixin
                       model:specification-mixin
                       model:conditional-mixin
                       model:parented-mixin
                       var:direct-variables-mixin)
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

(defmethod model:instantiate ((spec aspect-spec) &key parent specification-parent)
  (declare (ignore specification-parent))
  (aspects:make-aspect (aspect spec)
                       :name      (model:name spec)
                       :parent    parent
                       :variables (var:direct-variables spec)))

;;; `person'

(defclass person (rosetta-project.model.resource:person
                  var:direct-variables-mixin)
  ((explicit-names      :initarg  :explicit-names
                        :type     list
                        :reader   explicit-names
                        :initform '()
                        :documentation
                        "Stores a list of user-specified names,
                         ordered by preference.")
   (explicit-identities :initarg  :explicit-identities
                        :type     list
                        :reader   explicit-identities
                        :initform '()
                        :documentation
                        "Stores a list of user-specified identities,
                         ordered by preference."))
  (:documentation
   "Adds variables to the rosetta-project `person' class."))

(defmethod rosetta-project.model.resource:names ((thing person))
  (let ((all (append (explicit-names thing) (call-next-method))))
    (remove-duplicates all :test #'string= :from-end t)))

(defmethod rosetta-project.model.resource:identities ((thing person))
  (let ((all (append (explicit-identities thing) (call-next-method))))
    (remove-duplicates all :test #'puri:uri= :from-end t)))

(defmethod rosetta-project.model.resource:augment-person!
    ((person rosetta-project.model.resource:person) (other-person person))
  (rosetta-project.model.resource:augment-person!
   (change-class person 'person :variables           (var:direct-variables other-person)
                                :explicit-names      (explicit-names other-person)
                                :explicit-identities (explicit-identities other-person))
   other-person))
