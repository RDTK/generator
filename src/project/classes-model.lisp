;;;; classes-model.lisp --- Classes modeling projects, versions and jobs.
;;;;
;;;; Copyright (C) 2012, 2013, 2014 Jan Moringen
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
  (append (list :project-name (name thing))
          (when (next-method-p)
            (call-next-method))))

(defmethod add-dependencies! ((thing project) (spec project-spec))
  (mapc #'add-dependencies! (versions thing) (versions spec)))

(defmethod deploy ((thing project))
  (mapcar #'deploy (versions thing)))

;;; `version' class

(defclass version (named-mixin
                   implementation-mixin
                   direct-variables-mixin
                   parented-mixin)
  ((dependencies :initarg  :dependencies
                 :type     list
                 :reader   dependencies
                 :accessor %dependencies
                 :initform '()
                 :documentation
                 "

                  either a string naming the dependency or a list of
                  the form

                    (NAME VERSION)

                  .")
   (jobs         :initarg  :jobs
                 :type     list
                 :reader   jobs
                 :documentation
                 ""))
  (:documentation
   "Instances of this class represent versions of `project's."))

(defmethod direct-variables ((thing version))
  (append (list :version-name (name thing))
          (when (next-method-p)
            (call-next-method))))

(defmethod add-dependencies! ((thing version) (spec version-spec))
  (iter (for requires in (requires spec))
        (log:trace "~@<Trying to satisfy requirement ~S for ~A~:[~; ~
                    considering providers ~S~].~@:>"
                   requires thing providers-supplied? providers)
        (restart-case
            (let+ (((&flet instantiable-jobs (spec)
                      (remove-if-not (rcurry #'instantiate? spec) (jobs spec))))
                   ((&flet count-jobs (provider)
                      (length (intersection (instantiable-jobs spec)
                                            (instantiable-jobs provider)
                                            :test #'string= :key #'name))))
                   ((&flet+ order ((&ign . left-provider) (&ign . right-provider))
                      (> (count-jobs left-provider) (count-jobs right-provider))))
                   (candidate (implementation
                               (find-provider/version requires :order #'order))))
              (unless (eq candidate thing) ; TODO temp; should not happen
                (pushnew candidate (%dependencies thing))))
          (continue (&optional condition)
            :report (lambda (stream)
                      (format stream "~@<Skip requirement ~S.~@:>"
                              requires))
            (declare (ignore condition)))))

  (mapc #'add-dependencies! (jobs thing) (jobs spec)))

(defmethod deploy ((thing version))
  (mapcar #'deploy (jobs thing)))

;;; `job' class

(defclass job (named-mixin
               implementation-mixin
               specification-mixin ; TODO define another class for this
               direct-variables-mixin
               parented-mixin)
  ((dependencies :initarg  :dependencies ; TODO(jmoringe, 2013-03-06): dependencies-mixin?
                 :type     list ; of job
                 :reader   dependencies
                 :accessor %dependencies
                 :initform '()
                 :documentation
                 "List of other `job' instances.")
   (aspects      :initarg  :aspects
                 :type     list
                 :reader   aspects
                 :initform '()
                 :documentation
                 ""))
  (:documentation
   "Instances of this class represent build jobs which are associated
    to specific `version's of `project's."))

(defmethod direct-variables ((thing job))
  (append (list :job-name (name thing))
          (when (next-method-p)
            (call-next-method))))

(defmethod add-dependencies! ((thing job) (spec job-spec))
  (let ((dependency-name (or (ignore-errors
                              (value thing :dependency-job-name))
                             (name thing))))
    (iter (for dependency in (dependencies (parent thing)))
          (log:trace "~@<Trying to add ~A to ~A~@:>" dependency thing)
          (restart-case
              (let ((dependency
                      (or (find dependency-name (jobs dependency)
                                :test #'string= :key #'name)
                          (error "~@<Could not find ~S in the jobs of ~
                                  ~A~@[ (~{~A~^, ~})~]~@:>"
                                 dependency-name dependency
                                 (jobs dependency)))))
                (pushnew dependency (%dependencies thing)))
            (continue (&optional condition)
              :report (lambda (stream)
                        (format stream "~@<Skip adding dependency ~A.~@:>"
                                dependency))
              (declare (ignore condition)))))))

(defmethod deploy ((thing job))
  (let+ (((&flet format-description-part (stream part) ; TODO this whole description generation is a joke
            (pprint-fill stream (split-sequence:split-sequence #\Space part) nil)))
         ((&flet format-description ()
            (with-output-to-string (stream)
              (let ((*print-escape* nil)
                    (*print-right-margin* 80)
                    (*print-miser-width* nil))
                (when-let ((header (ignore-errors
                                    (value thing :description.header)))) ; TODO header is a hack
                  (format-description-part stream header)
                  (fresh-line stream))
                (format-description-part stream (value thing :description))
                (when-let ((footer (ignore-errors
                                    (value thing :description.footer))))
                  (fresh-line stream)
                  (format-description-part stream footer))))))
         (kind (make-keyword (string-upcase (value thing :kind))))
         (job  (jenkins.dsl:job (kind (value thing :bla-name)
                                      :description (format-description)))))
    (setf (%implementation thing) job)

    ;; Apply aspects, respecting declared ordering, and sort generated
    ;; builders according to declared ordering.
    (let ((*builder-constraints* (make-hash-table))
          (aspects (sort-with-partial-order (copy-list (aspects thing)) #'aspect<)))
      (reduce (rcurry #'extend! thing) aspects :initial-value job)

      (log:trace "Builder constraints: ~A" (hash-table-alist *builder-constraints*))

      (setf (builders job)
            (sort-with-partial-order
             (builders job) (rcurry #'builder< *builder-constraints*)))

      (log:trace "Sorted builders: ~A" (builders job)))

    ;; TODO temp
    (xloc:->xml job (stp:root (jenkins.api::%data job)) 'jenkins.api:job)

    (if (jenkins.api:job? (id job))
        (setf (jenkins.api:job-config (id job)) (jenkins.api::%data job))
        (jenkins.api::make-job (id job) (jenkins.api::%data job)))

    thing))

(defmethod deploy-dependencies ((thing job))
  (when (ignore-errors (value thing :no-dependencies))
    (return-from deploy-dependencies))

  (iter (for upstream-job in (dependencies thing))
        (restart-case
            (handler-bind
                ((error (lambda (condition)
                          (error "~@<Could not relate ~A -> ~A: ~A.~@:>"
                                 upstream-job thing condition))))
              ;; TODO make a function related? or add (relate .. :if-related nil)
              (unless (find (id (implementation thing))
                            (jenkins.api::downstream (implementation upstream-job))
                            :test #'string=)
                (jenkins.api:relate (implementation upstream-job) (implementation thing))))
          (continue (&optional condition)
            (declare (ignore condition))))))
