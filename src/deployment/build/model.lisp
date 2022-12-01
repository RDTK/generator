;;;; model.lisp --- Model used by the deployment.build module.
;;;;
;;;; Copyright (C) 2018, 2019, 2020, 2022 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:build-generator.deployment.build)

;;; `project-steps'

(defclass project-steps (model:implementation-mixin
                         aspects::aspect-builder-defining-mixin)
  ((%directory :initarg  :directory
               :reader   directory)
   (%steps     :initarg  :steps
               :accessor steps
               :initform '()))
  (:documentation
   "A collection of `step' instances for one project."))

(defun make-project-steps (specification directory)
  (make-instance 'project-steps :directory     directory
                                :specification specification))

(defmethod add-step ((step t) (steps project-steps))
  (push step (steps steps))
  (setf (%directory step) (directory steps))
  steps)

;;; `step'

(defclass step (deploy:command-mixin
                print-items:print-items-mixin)
  ((%name         :initarg  :name
                  :reader   name)
   (%dependencies :initarg  :dependencies
                  :accessor dependencies
                  :initform '())
   (%early?       :initarg  :early?
                  :type     boolean
                  :reader   early?
                  :initform nil
                  :documentation
                  "Controls whether the rule can be executed
                   \"early\", that is disregarding inter-project
                   dependencies.")
   (%directory    :initarg  :directory
                  :reader   directory
                  :writer   (setf %directory))
   ;; HACK
   (%builder-class :initarg :builder-class
                   :reader builder-class
                   :initform nil))
  (:default-initargs
   :name (more-conditions:missing-required-initarg 'step :name)))

(defun make-step (name command
                  &key (dependencies '()) early? directory builder-class)
  (make-instance 'step :name          name
                       :command       command
                       :dependencies  dependencies
                       :early?        early?
                       :directory     directory
                       :builder-class builder-class))

(defmethod print-items:print-items append ((object step))
  `(((:name (:before :command)) "~A " ,(name object))))
