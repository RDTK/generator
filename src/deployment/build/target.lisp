;;;; target.lisp --- Target definition for directly building software.
;;;;
;;;; Copyright (C) 2018-2023 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:build-generator.deployment.build)

;;; `build-target'

(defclass build-target ()
  ((working-directory         :initarg :working-directory
                              :type    (and pathname (satisfies uiop:directory-pathname-p))
                              :reader  working-directory
                              :documentation
                              #.(format nil "A directory into which ~
                                 temporary files such as repository ~
                                 checkouts should be placed."))
   (delete-working-directory? :initarg  :delete-working-directory?
                              :type     boolean
                              :reader   delete-working-directory?
                              :initform nil
                              :documentation
                              #.(format nil "Controls whether the ~
                                working directory should be deleted ~
                                after finishing the build.")))
  (:documentation
   #.(format nil "Build project versions contained in given ~
     distribution(s).~@
     ~@
     Build steps declared in the corresponding recipes are directly ~
     executed as sub-processes of the generator process:~@
     ~@
     * Project workspaces for repository checkouts and temporary files ~
       are created as sub-directories of the supplied working ~
       directory.~@
     ~@
     * Build steps are executed according to a schedule that respects ~
       dependencies between projects as well as ordering constraints ~
       between build steps of a single project.~@
     ~@
     * This schedule is executed in parallel according to the \"number ~
       of processes\" configuration option.~@
     ~@
     * Reports such as unit test results are not processed or ~
       generated.~@
     ~@
     * The build is not incremental. Each build is a full (re)build.")))

(service-provider:register-provider/class
 'deploy:target :build :class 'build-target)

;;; Deploying jobs and distributions

(defmethod deploy:deploy ((thing project::job) (target build-target))
  (let* ((directory (merge-pathnames (uiop:ensure-directory-pathname
                                      (deploy:job-full-name thing))
                                     (working-directory target)))
         (output    (make-project-steps thing directory)))
    (push output (model:implementations thing))

    ;; Apply aspects, respecting declared ordering. Translate builder
    ;; ordering constraints into step dependencies.
    (aspects:extend! (aspects:aspects thing) thing output :build)

    ;; Add a step for creating the build directory. Everything else
    ;; has to wait for that.
    (let ((ensure-directory (make-step "ensure-directory"
                                       (format nil "mkdir -p '~A'" directory)
                                       :early?    t
                                       :directory #P".")))
      (map nil (lambda (step)
                 (push ensure-directory (dependencies step)))
           (steps output))
      (push ensure-directory (steps output)))

    output))

(defmethod deploy:deploy ((thing sequence) (target build-target))
  (unless (every (of-type 'project:distribution) thing)
    (return-from deploy:deploy (call-next-method)))

  (let ((deployed-things (call-next-method)))
    ;; Translate inter-project dependencies into dependencies between
    ;; steps.
    (map nil (lambda (project-steps)
               (let ((steps (remove-if #'early? (steps project-steps))))
                 (map nil (lambda (dependency)
                            (let ((dependency-steps (model:implementation dependency)))
                              (map-product
                               (lambda (step dependency-step)
                                 (push dependency-step (dependencies step)))
                               steps (steps dependency-steps))))
                      (model:direct-dependencies (model:specification project-steps)))))
         deployed-things)

    ;; Execute the planned steps.
    (let ((working-directory (working-directory target))
          (steps             (mappend #'steps deployed-things)))
      (ensure-directories-exist working-directory)
      (unwind-protect
           (execute* (make-instance 'execution-state :pending-tasks steps))
        (when (delete-working-directory? target)
          (uiop:delete-directory-tree
           working-directory :validate (constantly t)))))))
