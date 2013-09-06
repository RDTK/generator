;;;; spec-classes.lisp ---
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
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

(cl:in-package #:jenkins.project)

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
              "")
   (jobs      :initarg  :jobs
              :type     list ; of job-spec
              :reader   jobs
              :accessor %jobs
              :initform '()
              :documentation
              ""))
  (:documentation
   "Instances of this class describe projects.

    `project-spec' instances can reference zero or more `template'
    instances from which variables, version specifications and job
    specifications are inherited.

    In addition, `project-spec' instances directly contain version and
    job specifications."))

(defmethod shared-initialize :after ((instance   project-spec)
                                     (slot-names t)
                                     &key)
  (iter (for job in (mappend #'jobs (templates instance)))
        (unless (find (name job) (%jobs instance)
                      :test #'string=
                      :key  #'name)
          (let ((job/clone (clone job)))
            (reinitialize-instance job/clone
                                   :parent instance)
            (appendf (%jobs instance) (list job/clone))))))

(defmethod direct-variables ((thing project-spec))
  (append (list :project-name (name thing))
          (when (next-method-p)
            (call-next-method))))

(defmethod variables append ((thing project-spec))
  ;; TODO(jmoringe, 2013-02-22): this is a hack to add our direct variables
  ;; in front of variables from templates. maybe variables should not
  ;; have `append' method combination?
  (append (direct-variables thing) (mapcan #'variables (templates thing))))

(defmethod aspects ((thing project-spec))
  (remove-duplicates (mappend #'aspects (templates thing))
                     :test #'string=
                     :key  #'name))

#+no (defmethod jobs ((thing project-spec))
  (remove-duplicates (append (direct-jobs thing)
                             (mapcan #'jobs (templates thing)))
                     :test     #'string=
                     :key      #'name
                     :from-end t))

(defmethod instantiate ((spec project-spec))
  (let+ (((&flet make-version (spec parent)
            (when (instantiate? spec parent)
              (when-let ((version (instantiate spec)))
                (list (reinitialize-instance version :parent parent))))))
         (name (name spec))
         (project (make-instance 'project
                                 :name      name
                                 :variables (variables spec) ; TODO(jmoringe, 2013-03-06): or direct-variables?
                                 )))
    (setf project
          (reinitialize-instance
           project
           :versions (mapcan (rcurry #'make-version project) (versions spec)))
          (find-instance name)
          project)))

;;; `version-spec' class

(defclass version-spec (named-mixin
                        specification-mixin
                        conditional-mixin
                        direct-variables-mixin
                        parented-mixin)
  ((requires     :initarg  :requires
                 :type     list
                 :accessor %requires
                 :initform '()
                 :documentation
                 "

                  either a string naming the dependency or a list of
                  the form

                    (MECHANISM NAME VERSION)

                 .")
   (provides     :initarg  :provides
                 :type     list
                 :reader   %provides
                 :initform '()
                 :documentation
                 "

                  either a string naming the dependency or a list of
                  the form

                    (MECHANISM NAME VERSION)

                  .")
   #+no (dependencies :initarg  :dependencies
                 :type     list
                 :reader   dependencies
                 :initform '()
                 :documentation
                 "

                  either a string naming the dependency or a list of
                  the form

                    (NAME VERSION)

                  ."))
  (:documentation
   "TODO(jmoringe): document"))

(defmethod shared-initialize :after ((instance   version-spec)
                                     (slot-names t)
                                     &key)
  ;; TODO moved from dump.lisp; but is not the right place, either
  (iter (for (mechanism name . version) in (provides instance))
        (push-provider
         instance
         (list* mechanism name
                (when (first version) (list (first version))))))

  (setf (%requires instance)
        (remove-if
         (lambda+ ((&ign name &optional version))
           (find (format nil "~(~A~@[-~A~]~)" (rs.f::normalize-name name) version)
                 (value instance :system-packages)
                 :test (lambda (a b) (ppcre:scan b a))))
         (%requires instance))))

(defmethod direct-variables ((thing version-spec))
  (append (list :version-name (name thing))
          (when (next-method-p)
            (call-next-method))))

(defmethod jobs ((thing version-spec))
  (jobs (parent thing)))

(defmethod requires :around ((spec version-spec))
  (remove-duplicates
   (call-next-method) :test #'equal :key (rcurry #'subseq 0 2)))

(defmethod provides :around ((spec version-spec))
  (remove-duplicates
   (call-next-method) :test #'equal :key (rcurry #'subseq 0 2)))

(let+ (((&flet+ parse-spec ((mechanism name &optional version))
          (list* (make-keyword (string-upcase mechanism))
                 name
                 (etypecase version
                   (list   version)
                   (string (list (jenkins.analysis:parse-version version))))))))

  (defmethod requires ((spec version-spec))
    (append (mapcar #'parse-spec (ignore-errors (value spec :extra-requires)))
            (%requires spec)))

  (defmethod provides ((spec version-spec))
    (append (mapcar #'parse-spec (ignore-errors (value spec :extra-provides)))
            (%provides spec))))

(defmethod requires-of-kind ((kind t) (spec version-spec))
  (remove kind (requires spec)
          :test (complement #'eq)
          :key  #'first))

(defmethod provides-of-kind ((kind t) (spec version-spec))
  (remove kind (provides spec)
          :test (complement #'eq)
          :key  #'first))

(defmethod instantiate ((spec version-spec))
  (let+ (((&flet make-job (spec parent)
            (when (instantiate? spec parent)
              (when-let ((job (instantiate spec)))
                (list (reinitialize-instance job :parent parent))))))
         (version (make-instance 'version
                                 :name      (name spec)
                                 :variables (direct-variables spec))))
    (reinitialize-instance
     version
     :jobs (mapcan (rcurry #'make-job version) (jobs spec)))))

;;; `job-spec' class

(defclass job-spec (named-mixin
                    specification-mixin
                    conditional-mixin
                    direct-variables-mixin
                    parented-mixin)
  ((tags :initarg  :tags
         :type     list
         :accessor tags
         :initform '()
         :documentation
         ""))
  (:documentation
   "TODO(jmoringe): document"))

(defmethod instantiate ((spec job-spec))
  (let+ (((&flet make-aspect (spec parent)
            (when (instantiate? spec parent)
              (when-let ((aspect (instantiate spec)))
                (list (reinitialize-instance aspect :parent parent))))))
         (job (make-instance 'job
                             :name      (name spec)
                             :variables (direct-variables spec))))
    (reinitialize-instance
     job
     :aspects   (mapcan (rcurry #'make-aspect job)
                        (applicable-aspects (parent spec) spec)))))

(defmethod clone ((thing job-spec))
  (make-instance 'job-spec
                 :name       (name thing)
                 :variables  (direct-variables thing)
                 :tags       (tags thing)
                 :conditions (conditions thing)))

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
            :reader   %direct-aspects
            :initform '()
            :documentation
            "")
   (jobs    :initarg  :jobs
            :type     list ; of job
            :reader   %direct-jobs
            :initform '()
            :documentation
            ""))
  (:documentation
   "TODO(jmoringe): document"))

;;; TODO(jmoringe, 2013-01-16): move to correct file

(defmethod direct-aspects ((thing template))
  (copy-list (%direct-aspects thing)))

(defmethod aspects ((thing template))
  (remove-duplicates (append (direct-aspects thing)
                             (mapcan (compose #'copy-list #'aspects) (inherit thing))) ; TODO(jmoringe, 2013-03-11): copy?
                     :test     #'string=
                     :key      #'name
                     :from-end t))

(defmethod direct-jobs ((thing template))
  (copy-list (%direct-jobs thing)))

(defmethod jobs ((thing template))
  (remove-duplicates (append (direct-jobs thing)
                             (mapcan (compose #'copy-list #'jobs) (inherit thing))) ; TODO(jmoringe, 2013-03-11): copy?
                     :test     #'string=
                     :key      #'name
                     :from-end t))

(defmethod applicable-aspects ((container t) (job job-spec))
  (remove-if (complement (rcurry #'aspect-applicable? job))
             (aspects container)))

;;; `aspect-spec'

(defclass aspect-spec (named-mixin
                       specification-mixin
                       conditional-mixin
                       direct-variables-mixin
                       parented-mixin)
  ((aspect :initarg  :aspect
           :type     string
           :reader   aspect
           :documentation
           "Name of the aspect class that should be used to implement
            this specification.")
   (filter :initarg  :filter
           :type     (or null string list)
           :reader   filter
           :initform nil
           :documentation
           ""))
  (:default-initargs
   :aspect (missing-required-initarg 'aspect-spec :aspect))
  (:documentation
   "TODO(jmoringe): document"))

;; TODO subsumed by instantiate? ?
(defmethod aspect-applicable? ((aspect aspect-spec)
                               (job    job-spec))
  (let+ (((&accessors-r/o filter) aspect)
         ((&labels apply-spec (spec)
            (etypecase spec
              (null
               t)
              (string
               (some (curry #'ppcre:scan spec) (tags job)))
              (list
               (every #'apply-spec spec))))))
    (apply-spec filter)))

(defmethod instantiate ((spec aspect-spec))
  ;; TODO(jmoringe, 2013-01-16): find-aspect-class
  (let* ((class-name (let ((*package* #.*package*))
                       (symbolicate '#:aspect- (string-upcase (aspect spec)))))
         (class      (find-class class-name)))
    (make-instance class
                   :name      (name spec)
                   :variables (direct-variables spec))))
