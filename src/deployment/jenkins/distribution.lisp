;;;; distribution.lisp --- Deployment of distributions as Jenkins jobs.
;;;;
;;;; Copyright (C) 2012-2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:build-generator.deployment.jenkins)

(defmethod deploy:deploy ((thing project:distribution) (target target))
  (let ((jobs               (call-next-method))
        (orchestration-jobs '())
        (views              '()))
    ;; Add dependencies for generated jobs.
    (when-let ((dependency-jobs
                (remove :none jobs
                        :key (rcurry #'var:value/cast :dependencies.mode))))
      (with-sequence-progress (:deploy/dependencies jobs)
        (map nil (lambda (job)
                   (progress "~/print-items:format-print-items/"
                             (print-items:print-items job))
                   (deploy:deploy-dependencies job target))
             dependency-jobs)))
    ;; Configure orchestration for the distribution.
    (with-simple-restart
        (continue "~@<Do not create orchestration jobs for ~
                   ~/print-items:format-print-items/~@:>"
                  (print-items:print-items thing))
      (setf orchestration-jobs (configure-orchestration thing target)))
    ;; Configure view(s) for the distribution.
    (with-simple-restart
        (continue "~@<Do not create a view for ~
                   ~/print-items:format-print-items/~@:>"
                  (print-items:print-items thing))
      (when (var:value/cast thing :view.create? nil)
        (let ((all-jobs (append jobs orchestration-jobs))
              (name     (var:value/cast thing :view.name))
              (columns  (var:value/cast thing :view.columns nil)))
          (push (apply #'configure-view name all-jobs
                       (when columns (list :columns columns)))
                views))))
    (values jobs orchestration-jobs views)))

;;; Orchestration

(defun configure-orchestration (distribution target)
  (with-trivial-progress (:orchestration "Configuring orchestration jobs")
    (let* ((templates    (list (project:find-template "orchestration")))
           (project-spec (make-instance 'project::project-spec
                                        :name      "orchestration"
                                        :templates templates))
           (version-spec (make-instance 'project::version-spec
                                        :name   "orchestration"
                                        :parent project-spec))
           (version      (progn
                           (reinitialize-instance project-spec
                                                  :versions (list version-spec))
                           (model:instantiate version-spec :parent distribution))))
      (flatten (deploy:deploy version target)))))

;;; Views

(defun configure-view (name jobs &key columns)
  (with-trivial-progress (:view "~A" name)
    (let ((jenkins-jobs (mappend #'model:implementations jobs))
          (view         (make-instance 'jenkins.api:view :id name)))
      (if (jenkins.api::view? name)
          (jenkins.api::update! view)
          (jenkins.api:make-view name (jenkins.api::%data view)))
      (setf (jenkins.api:jobs view) (mapcar #'jenkins.api:id jenkins-jobs))
      (when columns
        (setf (jenkins.api::columns view) columns))
      (jenkins.api:commit! view)
      view)))
