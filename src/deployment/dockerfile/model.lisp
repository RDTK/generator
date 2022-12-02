;;;; model.lisp --- Model for the dockerfile target.
;;;;
;;;; Copyright (C) 2018-2022 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:build-generator.deployment.dockerfile)

;;; Dockerfile
;;;
;;; A list of stages (can be a single stage) which are executed
;;; sequentially to build a particular image.

(defclass dockerfile ()
  ((%stages :initarg  :stages
            :type     list
            :reader   stages
            :initform '())))

;;; Stage
;;;
;;; Starts with a base image, performs a number of steps, makes the
;;; resulting image available for later stages.

(defclass stage-mixin ()
  ((%base-image   :initarg  :base-image
                  :reader   base-image)
   (%run-strategy :initarg  :run-strategy
                  :reader   run-strategy)
   (%steps        :initarg  :steps
                  :type     list
                  :reader   steps
                  :initform '())))

(defclass pseudo-stage (stage-mixin)
  ())

(defclass stage (model:named-mixin
                 stage-mixin)
  ())

;;; Step
;;;
;;; A somewhat self-contained operation that is executed as part of a
;;; stage to modify the state of the container in a particular way.

(defclass title-mixin ()
  ((%title :initarg :title
           :type    string
           :reader  title)))

(defclass command-step (title-mixin
                        deploy:command-mixin)
  ()
  (:documentation
   "Execute a command within a stage without writing a script file."))

(defclass script-step (model:named-mixin
                       title-mixin
                       deploy:command-mixin)
  ()
  (:documentation
   "Execute a command within a stage by writing a script file."))

(defclass copy-step ()
  ((%from-stage :initarg :from-stage
                :reader  from-stage)
   (%source     :initarg :source
                :type    string
                :reader  source)
   (%target     :initarg :target
                :type    string
                :reader  target))
  (:documentation
   "Copy results from an earlier stages."))

(defclass dockerfile-job (model:named-mixin
                          model:implementation-mixin
                          aspects::aspect-builder-defining-mixin)
  ((%builders :accessor builders
              :initform '()))
  (:documentation
   "Execute builders defined in a project description to process the
    project."))

;;; Builders
;;;
;;; A step that processes (that is builds, installs, etc.) a project
;;; is comprised of one or more "builders" which basically just
;;; execute shell commands.

(defclass shell-command (deploy:command-mixin
                         print-items:print-items-mixin)
  ((%aspect :initarg :aspect
            :reader  aspect)))

(defun shell-command (aspect format-control &rest format-arguments)
  (let ((command (if format-arguments
                     (apply #'format nil format-control format-arguments)
                     format-control)))
    (make-instance 'shell-command :aspect aspect :command command)))
