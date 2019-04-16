;;;; reference.lisp --- Reference contributors for different recipe types.
;;;;
;;;; Copyright (C) 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:build-generator.language-server)

;;; `template-reference-contributor'

(defclass template-reference-contributor ()
  ())

(defmethod contrib:reference-contributions
    ((workspace   t)
     (document    template-document)
     (context     t)
     (contributor template-reference-contributor))
  (when-let* ((template (object document))
              (name     (model:name template))
              (projects (projects workspace)))
    (mappend (lambda (project)
               (mappend (lambda (used-template)
                          (when (string= (model:name used-template) name)
                            (list (project::location-of project))))
                        (project:templates project)))
             projects)))

;;; `project-reference-contributor'

(defclass project-reference-contributor ()
  ())

(defmethod contrib:reference-contributions
    ((workspace   t)
     (document    project-document)
     (context     t) ; TODO restrict
     (contributor project-reference-contributor))
  (when-let* ((project       (object document))
              (name          (model:name project))
              (distributions (distributions workspace)))
    (mappend (lambda (distribution)
               (mappend (lambda (include)
                          (when (string= (project:project include) name)
                            (list (project::location-of include))))
                        (project:versions distribution)))
             distributions)))

;;; `variable-reference-contributor'

(defclass variable-reference-contributor ()
  ())


(labels ((references-in-collection (name collection)
           (mappend (lambda (object)
                      (mappend (lambda (entry)
                                 (when (string-equal (car entry) name)
                                   (list (project::location-of entry)
                                        ; (project::location-of (car entry))
                                         )))
                               (var:direct-variables object)))
                    collection))
         (all-references (name workspace)
           (nconc (when-let ((templates (templates workspace)))
                    (references-in-collection name (hash-table-values templates))) ; TODO
                  (when-let ((projects (projects workspace)))
                    (references-in-collection name projects))
                  (when-let ((distributions (distributions workspace)))
                    (references-in-collection name distributions)))))

  (defmethod contrib:reference-contributions
      ((workspace   t)
       (document    build-generator-document)
       (context     variable-name-context)
       (contributor variable-reference-contributor))
    (when-let ((name (prefix context)))
      (log:error name)
      (all-references name workspace)))

  (defmethod contrib:reference-contributions
      ((workspace   t)
       (document    build-generator-document)
       (context     variable-reference-context)
       (contributor variable-reference-contributor))
    (when-let ((name (variable-name context)))
      (log:error name)
      (all-references name workspace))))
