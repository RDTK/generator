;;;; definition.lisp --- Definition contributors for different recipe types.
;;;;
;;;; Copyright (C) 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:build-generator.language-server)

;;; `template-definition-contributor'

(defclass template-definition-contributor ()
  ())

(defmethod contrib:definition-contributions
    ((workspace   t)
     (document    build-generator-document)
     (context     template-name-context)
     (contributor template-definition-contributor))
  (when-let* ((name      (word context))
              (templates (let ((templates (templates workspace)))
                           (when (lparallel:fulfilledp templates)
                             (lparallel:force templates))))
              (template  (find name templates
                               :test #'string= :key #'model:name))
              (location  (project::location-of template)))
    (list location)))

;;; `project-definition-contributor'

(defclass project-definition-contributor ()
  ())

(defmethod contrib:definition-contributions
    ((workspace   t)
     (document    distribution-document)
     (context     project-name-context)
     (contributor project-definition-contributor))
  (when-let* ((name     (prefix context))
              (projects (let ((projects (projects workspace)))
                          (when (lparallel:fulfilledp projects)
                            (lparallel:force projects))))
              (project  (find name projects
                              :test #'string= :key #'model:name))
              (location (project::location-of project)))
    (list location)))
