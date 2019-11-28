;;;; defaults.lisp --- Default behavior of the deployment protocol.
;;;;
;;;; Copyright (C) 2012-2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:build-generator.deployment)

;;; `project:distribution'

(defmethod deploy ((thing project:distribution) (target t))
  (let ((versions (project:versions thing)))
    (with-sequence-progress (:deploy/project versions)
      (lparallel:pmapcan
       (lambda (version)
         (progress "~/print-items:format-print-items/"
                   (print-items:print-items version))
         (more-conditions::without-progress
           (with-simple-restart
               (continue "~@<Skip deploying project version ~S.~@:>" version)
             (flatten (deploy version target)))))
       :parts most-positive-fixnum versions))))

;;; `project:version'

(defvar *outermost-version?* t)

(defmethod deploy :around ((thing project:version) (target t))
  (if *outermost-version?*
      (with-condition-translation (((error project-deployment-error)
                                    :thing thing :target target))
        (let ((*outermost-version?* nil))
          (call-next-method)))
      (call-next-method)))

(defmethod deploy ((thing project:version) (target t))
  (let ((jobs (project:jobs thing)))
    (with-sequence-progress (:deploy/job jobs)
      (mappend (lambda (job)
                 (progress "~/print-items:format-print-items/"
                           (print-items:print-items job))
                 (list (deploy job target)))
               jobs))))
