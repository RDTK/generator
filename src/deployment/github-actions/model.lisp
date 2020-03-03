;;;; model.lisp --- Model for GitHub Actions.
;;;;
;;;; Copyright (C) 2020, 2021, 2022 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:build-generator.deployment.github-actions)

(defun id? (thing)
  (and (stringp thing)
       (every (lambda (character)
                (or (alphanumericp character)
                    (member character '(#\- #\_) :test #'char=)))
              thing)))

(deftype id ()
  '(and string (satisfies id?)))

;;; `worklfow'

(defclass workflow (utilities.print-items:print-items-mixin)
  ((%name :initarg  :name
          :type     id
          :reader   name)
   (%jobs :initarg  :jobs
          :type     list
          :accessor jobs
          :initform '())))

(defmethod print-items:print-items append ((object workflow))
  (let ((job-count (length (jobs object))))
    `((:name                       "~A"         ,(name object))
      ((:job-count (:after :name)) " ~D job~:P" ,job-count))))

;;; `job'

(defclass job (model:named-mixin
               model:implementation-mixin
               utilities.print-items:print-items-mixin)
  ((%needs   :initarg  :needs
             :type     list
             :accessor needs
             :initform '())
   (%runs-on :initarg  :runs-on
             :type     string
             :accessor runs-on)
   (%steps   :initarg  :steps
             :type     list
             :accessor steps
             :initform '())
   (%timeout :initarg  :timeout
             :accessor timeout
             :initform nil))
  (:default-initargs
   :runs-on (missing-required-initarg 'job :runs-on)))

(defmethod print-items:print-items append ((object job))
  (let ((step-count (length (steps object))))
    `(((:step-count (:after :name)) " ~D step~:P" ,step-count))))

;;; `step'

(defclass step (model:named-mixin
                utilities.print-items:print-items-mixin)
  (;; HACK
   (%builder-class :initarg :builder-class
                   :reader builder-class
                   :initform nil)))

;;; Command step

(defclass command-step (step
                        deploy:command-mixin)
  ((%environment :initarg  :environment
                 :accessor environment
                 :initform nil))
  (:default-initargs
   :command (missing-required-initarg 'step :command)))

(defun command-step (name command &key (builder-class 'aspects::shell)) ; TODO builder-class is a hack
  (make-instance 'command-step :name name :command command :builder-class builder-class))

;;; Action step

(defclass action-step-mixin ()
  ((%uses       :initarg  :uses
                :reader   uses))
  (:default-initargs
   :uses (missing-required-initarg 'action-step-mixin :uses)))

(defclass action-step (action-step-mixin
                       step)
  ((%parameters :initarg  :parameters
                :type     list
                :reader   parameters
                :initform '())))

;;; Checkout step

(defclass checkout-step (action-step-mixin)
  ((%repository :initarg  :repository
                :reader   repository)
   (%ref        :initarg  :ref
                :reader   ref
                :initform nil)
   (%depth      :initarg  :depth
                :reader   depth
                :initform nil))
  (:default-initargs
   :uses "actions/checkout@v3"))

(defun make-github-checkout (name repository &optional ref)
  (make-instance 'checkout-step :name       name
                                :repository repository
                                :ref        ref
                                :builder-class 'aspects::git)) ; TODO needed?

;;; Up/download artifact step

(defun make-upload-artifact (artifact-name source-directory)
  (let ((step-name (format nil "upload-artifact-~A" artifact-name)))
    (make-instance 'action-step :name       step-name
                                :uses       "actions/upload-artifact@v1"
                                :parameters (list :name artifact-name
                                                  :path source-directory))))

(defun make-download-artifact (artifact-name target-directory)
  (let ((step-name (format nil "download-artifact-~A" artifact-name)))
    (make-instance 'action-step :name       step-name
                                :uses       "actions/download-artifact@v1"
                                :parameters (list :name artifact-name
                                                  :path target-directory))))
