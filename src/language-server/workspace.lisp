;;;; workspace.lisp --- TODO.
;;;;
;;;; Copyright (C) 2016, 2017, 2018, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.language-server)

(defclass workspace (lsp:workspace
                     lsp:document-container-mixin
                     lsp:root-uri-mixin
                     print-items:print-items-mixin)
  ((%templates :accessor %templates
               :initform nil)
   (%projects  :accessor %projects
               :initform nil)))

(defmethod load-templates ((container workspace))
  (let ((jenkins.model.project::*templates* (make-hash-table :test #'equal))
        (jenkins.model.project::*templates-lock* (bt:make-lock))
        (pattern (merge-pathnames "templates/toolkit/*.template"
                                  (lsp:root-directory container))))
    (log:error "Background-loading templates from ~A" pattern)
    (map nil #'jenkins.model.project:load-template/yaml (directory pattern))
    jenkins.model.project::*templates*))

(defmethod ensure-templates ((container workspace))
  (loop :for templates = (%templates container)
        :when templates :do (return templates)
        :do (let ((promise (lparallel:promise)))
              (when (null (sb-ext:compare-and-swap
                           (slot-value container '%templates) nil promise))
                ;; (methods::log-message (proto:make-message :info "Background-loading templates"))
                (lparallel:future (lparallel:fulfill promise
                                    (load-templates container)))))))

(defmethod templates ((container workspace))
  (let ((templates (ensure-templates container)))
    (if (lparallel:fulfilledp templates)
        (hash-table-values (lparallel:force templates))
        (lparallel:future (hash-table-values (lparallel:force templates))))))

(defmethod find-template ((name t) (container workspace))
  (let ((templates (ensure-templates container)))
    (if (lparallel:fulfilledp templates)
        (gethash name (lparallel:force templates))
        (lparallel:future (gethash name (lparallel:force templates))))))

(defmethod load-projects ((container workspace))
  (let ((jenkins.model.project::*templates* (lparallel:force (ensure-templates container)))
        (jenkins.model.project::*projects* nil ; (make-hash-table :test #'equal)
                                           )
        (jenkins.model.project::*projects-lock* (bt:make-lock))
        (pattern (merge-pathnames "projects/*.project" (lsp:root-directory container))))
    (handler-bind (((and error jenkins.util:continuable-error)
                     (compose #'invoke-restart #'jenkins.util:find-continue-restart)))
      (mappend (lambda (filename)
                 (with-simple-restart (continue "Skip")
                   (list (jenkins.model.project:load-project-spec/yaml filename))))
               (directory pattern)))))

(defmethod ensure-projects ((container workspace))
  (loop :for projects = (%projects container)
        :when projects :do (return projects)
        :do (let ((promise (lparallel:promise)))
              (when (null (sb-ext:compare-and-swap
                           (slot-value container '%projects) nil promise))
                ; (methods::log-message (proto::make-message :info "Background-loading projects"))
                (lparallel:future (lparallel:fulfill promise
                                    (load-projects container)))))))

(defmethod projects ((container workspace))
  (ensure-projects container))

#+no (defun all-keywords ()
  (remove-duplicates
   (mappend (lambda (project)
              (with-simple-restart (continue "Skip")
                (jenkins.model.variables:value/cast project :keywords '())))
            (projects container))
   :test #'string=))

(defvar *connection*)
(defvar *uri* nil)
(defvar *version* nil)

(defmethod lsp:process-interface-method :around ((object    workspace)
                                                 (interface (eql :textdocument))
                                                 (method    t)
                                                 &key
                                                 text-document)
  (let ((*uri* (assoc-value text-document :uri)))
    (if (member method '(:didopen :didclose))
        (call-next-method)
        (let ((*version* (lsp:version (lsp:find-document *uri* object))))
          (call-next-method)))))

(defmethod lsp:make-document ((container workspace)
                              (language  t)
                              (version   t)
                              (text      t))
  (make-instance (ecase language
                   (:template-recipe     'template-document)
                   (:project-recipe      'project-document)
                   (:distribution-recipe 'distribution-document))
                 :language  language
                 :version   version
                 :text      text
                 :workspace container))
