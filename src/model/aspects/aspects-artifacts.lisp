;;;; aspects-artifacts.lisp --- Definitions of artifact-related aspects
;;;;
;;;; Copyright (C) 2012-2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:build-generator.model.aspects)

;;; Archive artifact

(define-aspect (archive-artifacts :job-var job) (publisher-defining-mixin)
    (((file-pattern (bail)) :type (or null (var:list-of string) string)
      :documentation
      "An Ant-style file pattern designating the files that should be
       archived.

       See Jenkins documentation for details."))
  "Adds an artifact archiving publisher to the generated job.

   When multiple instances of this aspect are applied to a single job,
   the union of the respective FILE-PATTERNs is configured as the
   pattern of files to archive."
  (removef (jenkins.api:publishers job) 'jenkins.api:publisher/archive-artifacts
           :key #'type-of)
  (when file-pattern
    (push (constraint! (publish)
            (make-instance 'jenkins.api:publisher/archive-artifacts
                           :files        (ensure-list file-pattern)
                           :only-latest? nil))
          (jenkins.api:publishers job))))

;;; Dependency download aspect

(define-aspect (dependency-download :job-var  job
                                    :spec-var spec
                                    :plugins  ("copyartifact"))
    (builder-defining-mixin)
    ((((:upstream-dir upstream-dir)) :type string))
  "Configures artifact downloads for the generated job.

   Copy-artifact actions are generated to copy artifacts from all
   suitable jobs generated for projects in the transitive
   upstream-closure (i.e. upstream projects, upstream projects of
   upstream projects, etc.). A job from this set is suitable if it is
   configured to archive any artifacts. Such artifacts will be copied
   into the directory designated by the upstream-dir parameter and
   extracted.

   Note: An error is signaled if an attempt is made to copy artifacts
   from a matrix upstream project into a non-matrix downstream
   project. The combinations
     matrix     -> matrix
     non-matrix -> matrix
     non-matrix -> non-matrix
   are permitted.

   Note: Only useful in continuous integration operation modes that do
   not install built software but copy results from upstream jobs into
   the workspaces of downstream jobs."
  (let ((copy-artifacts? nil))
    (when-let ((self-kind    (first (ensure-list (var:value spec :kind nil))))
               (dependencies (model:dependencies spec)))
      ;; Multiple copy-artifact builders which copy artifacts from
      ;; other jobs.
      (iter (for dependency in dependencies)
            (let+ ((id      (var:value/cast dependency :build-job-name))
                   (kind    (first (ensure-list (var:value dependency :kind nil))))
                   (pattern (when-let ((aspect (find-if (of-type 'aspect-archive-artifacts)
                                                        (aspects dependency))))
                              (var:value/cast aspect :aspect.archive-artifacts.file-pattern nil)))
                   ((&flet matrix? (kind)
                      (member kind '("matrix" "matrix-project") :test #'string-equal)))
                   (reference (format nil "~A~@[/label=$label~]"
                                      id (matrix? kind))))
              (cond
                ((not pattern)
                 (log:info "~@<Upstream project ~A does not provide ~
                             archived artifacts to copy into downstream ~
                             workspaces (variable ~S has no value).~@:>"
                  dependency :aspect.archive-artifacts.file-pattern))
                ((and (matrix? kind) (not (matrix? self-kind)))
                 (error "~@<Upstream job ~A is of kind ~A, downstream ~
                          job ~A is of kind ~A.~@:>"
                  dependency kind job self-kind))
                (t
                 (push (constraint! (build ((:after sloccount))
                                           copy-artifact)
                         (make-instance 'jenkins.api:builder/copy-artifact
                                        :project-name reference
                                        :filter       (ensure-list pattern)
                                        :target       upstream-dir
                                        :flatten?     t
                                        :clazz        "hudson.plugins.copyartifact.StatusBuildSelector"))
                       (jenkins.api:builders job))
                 (setf copy-artifacts? t)))))

      ;; Shell builder which unpacks dependencies. Has to run after
      ;; artifact download, obviously.
      (when copy-artifacts?
        (let ((command (format nil "cd ~A~@
                                    find . -name '*.tar.gz' -exec tar -xzf '{}' \\;"
                               upstream-dir)))
          (push (constraint! (build ((:before cmake/unix)
                                     (:after copy-artifact)))
                  (make-instance 'jenkins.api:builder/shell :command command))
                (jenkins.api:builders job)))))))
