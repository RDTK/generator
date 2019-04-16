;;;; definition.lisp --- Definition contributors for different recipe types.
;;;;
;;;; Copyright (C) 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.language-server)

;;; `template-definition-contributor'

(defclass template-definition-contributor ()
  ())

(defmethod contrib:definition-contributions
    ((workspace   t)
     (document    build-generator-document)
     (context     template-name-context)
     (contributor template-definition-contributor))
  (when-let* ((name     (word context))
              (template (find-template name workspace))
              (location (project::location-of template)))
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
              (project  (find-project name workspace))
              (location (project::location-of project)))
    (list location)))
