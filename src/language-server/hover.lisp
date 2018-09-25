;;;; hover.lisp --- TODO.
;;;;
;;;; Copyright (C) 2016, 2017, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.language-server)

(defclass variable-hover-contributor () ())

(defmethod contrib:hover-contribution ((workspace   t)
                                       (document    t)
                                       (context     variable-value-context)
                                       (contributor variable-hover-contributor))
  (let+ (((&accessors-r/o variable-location variable-node) context))
    (values
     (list (format nil "Type: ~A" (jenkins.model.variables:variable-info-type variable-node))
           (or (jenkins.model.variables:variable-info-documentation variable-node)
               "«undocumented variable»"))
     (text.source-location:range variable-location) )))

;;;

(defclass project-version-hover-contributor () ())

(defmethod contrib:hover-contribution
    ((workspace   t)
     (document    t)
     (context     project-name-context)
     (contributor project-version-hover-contributor))
  (let ((prefix   (prefix context))
        (projects (projects (workspace document))))
    (when (lparallel:fulfilledp projects)
      (map nil (lambda (project)
                 (let ((name (jenkins.model:name project)))
                   (when (starts-with-subseq prefix name)
                     (return-from contrib:hover-contribution
                       (values (describe-project project))))))
           (lparallel:force projects)))))
