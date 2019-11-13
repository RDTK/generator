;;;; symbols.lisp --- Symbols contributors for recipes.
;;;;
;;;; Copyright (C) 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:build-generator.language-server)

;; TODO contributor
(labels ((object->document-symbol (name kind object document &optional (children '()))
           (when-let ((location (project::location-of
                                 object (locations document))))
             (list (proto:make-document-symbol
                    name kind (sloc:range location) :children children))))
         (variables->document-symbols (object document)
           (loop :for cell :in (var:direct-variables object)
                 :for (name . nil) = cell
                 :appending (object->document-symbol
                             (string-downcase (symbol-name name)) :variable cell document))))

  (defmethod methods:symbols ((workspace workspace) (document project-document))
    (when-let ((object (object document)))
      (object->document-symbol
       (model:name object) :module object document
       (append (variables->document-symbols object document)
               (mappend (lambda (version)
                          (object->document-symbol
                           (model:name version) :method ; :object TODO hack for emacs
                           version document
                           (variables->document-symbols version document)))
                        (project:versions object))))))

  (defmethod methods:symbols ((workspace workspace)
                              (document  distribution-document))
    (when-let ((object (object document)))
      (object->document-symbol
       (model:name object) :module object document
       (append (variables->document-symbols object document)
               (mappend (lambda (distribution-include)
                          (object->document-symbol
                           (model:name (project:distribution distribution-include))
                           :class       ; TODO :module hack for emacs
                           distribution-include document
                           (variables->document-symbols distribution-include document)))
                        (project:direct-includes object))
               (mappend (lambda (project-include)
                          (object->document-symbol
                           (format nil "~{~A~^:~}"
                                   (reverse (model:ancestor-names
                                             (project:version project-include))))
                           :method      ; TODO :object hack for emacs
                           (model:specification project-include) document
                           (variables->document-symbols project-include document)))
                        (project:direct-versions object)))))))
