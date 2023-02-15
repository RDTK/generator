;;;; target.lisp --- Target definition for generating a Dockerfiles.
;;;;
;;;; Copyright (C) 2018-2023 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:build-generator.deployment.dockerfile)

(defclass dockerfile-target ()
  (;; Configuration
   (output-directory :initarg  :output-directory ; TODO mixin to share with makefile-target
                     :type     (and pathname (satisfies uiop:directory-pathname-p))
                     :reader   output-directory
                     :documentation
                     #.(format nil "The directory into which the ~
                        Dockerfile and the associated scripts should ~
                        be written."))
   (staging-image    :initarg  :staging-image
                     :type     (or null string)
                     :reader   staging-image
                     :initform nil
                     :documentation
                     #.(format nil "The Docker image in which builds ~
                        should be performed.~@
                        ~@
                        Dependencies installed into and intermediate ~
                        results built in the corresponding container ~
                        can be omitted from the final image to save ~
                        space."))
   (base-image       :initarg  :base-image
                     :type     string
                     :reader   base-image
                     :documentation
                     #.(format nil "The Docker image on which the ~
                        resulting image should be based."))
   (platform         :initarg  :platform
                     :type     (or null (cons string))
                     :reader   platform
                     :writer   (setf %platform)
                     :initform nil
                     :documentation
                     #.(format nil "The target platform assumed in ~
                        platform dependency computations.~@
                        ~@
                        If not specified, an attempt is made to derive ~
                        the platform from the specified base image.~@
                        ~@
                        It is a fatal error if the platform is not ~
                        specified and cannot be derived from the base ~
                        image."))
   (run-strategy     :initarg  :run-strategy
                     :type     (member :one-file-for-all-builders
                                       :one-file-per-builder)
                     :reader   run-strategy
                     :initform :one-file-per-builder
                     :documentation
                     #.(format nil "Controls whether the commands for ~
                        one project are executed as a single RUN ~
                        command or multiple RUN commands.~@
                        ~@
                        A single RUN command is more efficient in ~
                        terms of image build time and the storage layout ~
                        of the resulting image.~@
                        ~@
                        Multiple RUN commands are better for debugging ~
                        and interactive development since build steps ~
                        can be cached with a finer granularity.")))
  (:default-initargs
   :output-directory (more-conditions:missing-required-initarg 'dockerfile-target :output-directory)
   :base-image       (more-conditions:missing-required-initarg 'dockerfile-target :base-image))
  (:documentation
   "Generate a Dockerfile that builds projects into an image."))

(service-provider:register-provider/class
 'deploy:target :dockerfile :class 'dockerfile-target)

(defmethod initialize-instance :after ((instance dockerfile-target) &key)
  (when (null (platform instance))
    (let ((base-image (base-image instance)))
     (if-let ((index (position #\: base-image)))
       (setf (%platform instance) (list (subseq base-image 0 index)
                                        (subseq base-image (1+ index))))
       (error "~@<Cannot guess platform from base image ~S. Supply a ~
               platform explicitly.~@:>"
              base-image)))))

;;; Stage-level processing

(defun make-package-installation-step (distribution target)
  (let* ((platform     (platform target))
         (title        (format nil "Dependencies for platform ~{~A~^ ~}"
                               platform))
         (requirements (project:platform-requires distribution platform))
         (apt-command  "DEBIAN_FRONTEND=noninteractive apt-get -qq")
         (command      (if requirements
                           (format nil "~A update \\~@
                                        ~4T&& ~:*~A --assume-yes upgrade \\~@
                                        ~4T&& ~:*~A --assume-yes install \\~@
                                        ~6@T~{~<~T\\~%~6@T~1,:;~A~>~^ ~} \\~@
                                        ~4T&&~2:*~A clean"
                                   apt-command requirements)
                           "# No dependencies")))
    (make-instance 'command-step :title title :command command)))

(defun make-hook-step (distribution which)
  (when-let* ((command (var:value distribution which nil))
              (name    (model:name distribution)))
    (let ((name  (format nil "~A-~(~A~)" name which))
          (title (format nil "~A for distribution ~A" which name)))
      (make-instance 'script-step :name    name
                                  :title   title
                                  :command command))))

(defun make-direct-process (base-image dependencies prepare build finish
                            run-strategy)
  (let* ((steps (append dependencies prepare build finish))
         (build (make-instance 'pseudo-stage :base-image   base-image
                                             :run-strategy run-strategy
                                             :steps        steps)))
    (make-instance 'dockerfile :stages (list build))))

(defun make-staging-process (staging-image staging-directory
                             base-image    target-directory
                             dependencies prepare build finish run-strategy)
  (let* ((build-steps (append dependencies prepare build))
         (build       (make-instance 'stage :name         "Build"
                                            :base-image   staging-image
                                            :run-strategy run-strategy
                                            :steps        build-steps))
         (copy        (make-instance 'copy-step :from-stage build
                                                :source     staging-directory
                                                :target     target-directory))
         (final-steps (append dependencies (list copy) finish))
         (final       (make-instance 'stage :name         "Final"
                                            :base-image   base-image
                                            :run-strategy run-strategy
                                            :steps        final-steps)))
    (make-instance 'dockerfile :stages (list build final))))

(defmethod deploy:deploy ((thing sequence) (target dockerfile-target))
  (unless (every (of-type 'project:distribution) thing)
    (return-from deploy:deploy (call-next-method)))

  (let+ (((&accessors-r/o staging-image base-image run-strategy) target)
         (deployed-things    (call-next-method))
         (deployed-things    (util:sort-with-partial-order
                              deployed-things (lambda (left right)
                                                (find (model:specification left)
                                                      (model:dependencies
                                                       (model:specification right))))))
         (output-directory   (output-directory target))
         (dockerfile         (merge-pathnames "Dockerfile" output-directory))
         (first-distribution (first-elt thing))
         (target-directory   (var:value first-distribution :toolkit.dir))
         (dependencies       (list (make-package-installation-step
                                    first-distribution target))) ; TODO multiple distributions?
         (prepare            (when-let ((step (make-hook-step first-distribution :prepare-hook/unix)))
                               (list step)))
         (build              deployed-things)
         (finish             (when-let ((step (make-hook-step first-distribution :finish-hook/unix)))
                               (list step)))
         (model              (cond (staging-image
                                    (make-staging-process
                                     staging-image target-directory
                                     base-image    target-directory
                                     dependencies prepare build finish
                                     run-strategy))
                                   (t
                                    (make-direct-process
                                     base-image
                                     dependencies prepare build finish
                                     run-strategy))))
         (context            (make-instance 'context :directory    output-directory
                                                     :dockerfile   dockerfile
                                                     :run-strategy run-strategy)))
    (output model context)))

;;; Step-level processing

(defmethod aspects::step-constraints ((aspect aspects::aspect-builder-defining-mixin)
                                      (phase  (eql 'aspects::build))
                                      (step   shell-command))
  (let* ((variable        :aspect.builder-constraints.shell)
         (constraints/raw (var:value aspect variable nil))
         (constraints     (mapcar #'aspects::parse-constraint constraints/raw)))
    (log:trace "~@<Constraints for ~A in ~A~:@_~
                ~/aspects::format-constraints/~@:>"
               step variable constraints)
    constraints))

(defmethod deploy:deploy ((thing project::job) (target dockerfile-target))
  (let* ((name   (deploy:job-full-name thing))
         (output (make-instance 'dockerfile-job :name          name
                                                :specification thing)))
    (push output (model:implementations thing))

    ;; Apply aspects, respecting declared ordering, and sort generated
    ;; builders according to declared ordering.
    (aspects:extend! (aspects:aspects thing) thing output :dockerfile)

    output))
