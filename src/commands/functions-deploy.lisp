;;;; functions-deploy.lisp --- Functions for deploying Jenkins jobs.
;;;;
;;;; Copyright (C) 2017, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.project.commands)

;;; Deployment

(defun instantiate-projects (specs
                             &optional
                             (distributions nil distributions-supplied?))
  (let+ ((projects (mapcar (lambda (spec)
                             (instantiate spec :parent (parent spec)))
                           specs))
         ((&flet find-version (version)
            (when-let ((dist (find version distributions
                                   :test #'member
                                   :key  #'versions)))
              (remove-if-not (rcurry #'member (versions dist))
                             (providers/alist)
                             :key #'cdr)))))
    (iter (for project in projects)
          (for spec    in specs)
          (when project
            (apply #'add-dependencies! project spec
                   (when distributions-supplied?
                     (list :providers #'find-version)))
            (collect project)))))

(defun deploy-projects (projects)
  (with-sequence-progress (:deploy/project projects)
    (iter (for project in projects)
          (progress "~/print-items:format-print-items/"
                    (print-items:print-items project))
          (more-conditions::without-progress
            (with-simple-restart
                (continue "~@<Skip deploying project ~S.~@:>" project)
              (appending (flatten (deploy project))))))))

(defun deploy-job-dependencies (jobs)
  (with-sequence-progress (:deploy/dependencies jobs)
    (iter (for job in jobs)
          (progress "~/print-items:format-print-items/"
                    (print-items:print-items job))
          (deploy-dependencies job))))

;;; Toolkit specific stuff

(defun configure-orchestration (distribution)
  (with-trivial-progress (:orchestration "Configuring orchestration jobs")
    (let* ((templates (list (find-template "orchestration")))
           (spec      (make-instance 'jenkins.model.project::project-spec
                                     :name      "orchestration"
                                     :parent    distribution
                                     :templates templates))
           (version   (make-instance 'jenkins.model.project::version-spec
                                     :name   "orchestration"
                                     :parent spec)))
      (reinitialize-instance spec :versions (list version))
      (flatten (deploy (instantiate spec))))))

(defun configure-view (name jobs &key columns)
  (with-trivial-progress (:view "~A" name)
    (let ((view (make-instance 'jenkins.api:view :id name)))
      (if (jenkins.api::view? name)
          (jenkins.api::update! view)
          (jenkins.api:make-view name (jenkins.api::%data view)))
      (setf (jenkins.api:jobs view) (mapcar #'jenkins.api:id jobs))
      (when columns
        (setf (jenkins.api::columns view) columns))
      (jenkins.api:commit! view)
      view)))

(defun configure-distribution (distribution)
  (let* ((jobs               (mappend (compose #'jobs #'implementation)
                                      (versions distribution)))
         (orchestration-jobs (with-simple-restart
                                 (continue "~@<Continue without configuring orchestration jobs~@:>")
                               (configure-orchestration distribution)))
         (all-jobs           (mapcar #'implementation
                                     (append jobs orchestration-jobs))))
    (log:trace "~@<Jobs in ~A: ~A~@:>" distribution jobs)
    (when-let* ((create? (value/cast distribution :view.create? nil))
                (name    (value/cast distribution :view.name)))
      (let ((columns (value/cast distribution :view.columns nil)))
        (with-simple-restart (continue "~@<Continue without creating a view~@:>")
          (apply #'configure-view name all-jobs
                 (when columns (list :columns columns))))))
    (values jobs orchestration-jobs all-jobs)))

(defun configure-distributions (distributions)
  (values-list
   (reduce (lambda+ ((jobs orchestration-jobs all-jobs) distribution)
             (let+ (((&values jobs1 orchestration-jobs1 all-jobs1)
                     (configure-distribution distribution)))
               (list (append jobs1               jobs)
                     (append orchestration-jobs1 orchestration-jobs)
                     (append all-jobs1           all-jobs))))
           distributions
           :initial-value '(() () ()))))

(defun list-credentials (jobs)
  (let+ ((all-credentials (make-hash-table :test #'equal))
         ((&flet+ job-credentials (job)
            (when-let* ((repository  (jenkins.api:repository job))
                        (credentials (jenkins.api:credentials repository)))
              (push job (gethash credentials all-credentials))))))
    (mapc #'job-credentials jobs)
    (when (plusp (hash-table-count all-credentials))
      (format t "~@<The following credentials have been referenced and ~
                 have to be configured in Jenkins' credential store:~@:_~
                 ~{~{* ~S for job~P ~<~{~A~^, ~}~:@>~}~^~@:_~}~
                 ~%~:>"
              (mapcar (lambda+ ((credentials . jobs))
                        (list credentials (length jobs)
                              (list (mapcar #'jenkins.api:id jobs))))
                      (hash-table-alist all-credentials))))))
