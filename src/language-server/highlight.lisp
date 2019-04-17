;;;; highlight.lisp --- Highlight contributors for different recipe types.
;;;;
;;;; Copyright (C) 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.language-server)

;;; `variable-highlight-contributor'

(defclass variable-highlight-contributor ()
  ())

(defmethod contrib:document-highlight-contributions
    ((workspace   t)
     (document    build-generator-document)
     (context     variable-reference-context)
     (contributor variable-highlight-contributor))
  (append (list (proto:make-highlight :text (prefix-range context)))
          (when-let* ((definition (definition context))
                      (location   (project::location-of definition (locations document))))
            (when t ; (eq (sloc:source location) (source document))
             (list (proto:make-highlight :write (sloc:range location)))))))
