;;;; defaults.lisp --- Default behavior of the deployment protocol.
;;;;
;;;; Copyright (C) 2012-2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:build-generator.deployment)

;;; `project:distribution'

(defmethod deploy ((thing project:distribution))
  (let ((versions (project:versions thing)))
    (with-sequence-progress (:deploy/project versions)
      (mappend (lambda (version)
                 (progress "~/print-items:format-print-items/"
                           (print-items:print-items version))
                 (more-conditions::without-progress
                   (with-simple-restart
                       (continue "~@<Skip deploying project version ~S.~@:>" version)
                     (flatten (deploy version)))))
               versions))))

;;; `project:version'

(defvar *outermost-version?* t)

(defmethod deploy :around ((thing project:version))
  (if *outermost-version?*
      (with-condition-translation (((error project-deployment-error)
                                    :thing thing))
        (let ((*outermost-version?* nil))
          (call-next-method)))
      (call-next-method)))

(defmethod deploy ((thing project:version))
  (let ((jobs (project:jobs thing)))
    (with-sequence-progress (:deploy/job jobs)
      (mappend (lambda (job)
                 (progress "~/print-items:format-print-items/"
                           (print-items:print-items job))
                 (list (deploy job)))
               jobs))))
