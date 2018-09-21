;;;; workspace.lisp --- TODO.
;;;;
;;;; Copyright (C) 2016, 2017, 2018, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:build-generator.language-server)

(defun all-keywords ()
 (handler-bind ((error #'continue))
   (let* ((jenkins.model.project::*templates* (make-hash-table :test #'equal))
          (projects                           (progn
                                                (map nil #'jenkins.model.project:load-template/yaml
                                                     (directory "~/code/citec/citk/recipes/templates/toolkit/*.template"))
                                                (mappend (lambda (filename)
                                                           (with-simple-restart (continue "Skip")
                                                             (list (jenkins.model.project:load-project-spec/yaml filename))))
                                                         (directory "~/code/citec/citk/recipes/projects/*.project")))))
     (remove-duplicates
      (mappend (lambda (project)
                 (with-simple-restart (continue "Skip")
                   (jenkins.model.variables:value/cast project :keywords '())))
               projects)
      :test #'string=))))

(defclass workspace (lsp:standard-workspace)
  ((%templates :accessor %templates
               :initform nil)))

(defmethod load-templates ((container workspace))
  (let ((project::*templates* (make-hash-table :test #'equal))
        (project::*templates-lock* (bt:make-lock))
        (pattern (merge-pathnames "../templates/toolkit/*.template"
                                  (lsp:root-path container))))
    (log:error "Background-loading templates from ~A" pattern)
    (map nil #'project:load-template/yaml (directory pattern))
    project::*templates*))

(defmethod ensure-templates ((container workspace))
  (loop :for templates = (%templates container)
        :when templates :do (return (lparallel:force templates))
        :do (let ((promise (lparallel:promise)))
              (when (null (sb-ext:compare-and-swap
                           (slot-value container '%templates) nil promise))
                (methods::log-message (proto:make-message :info "Background-loading templates"))
                (lparallel:future (lparallel:fulfill promise
                                    (load-templates container)))))))

(defmethod templates ((container workspace))
  (hash-table-values (ensure-templates container)))

(defmethod find-template ((name t) (container workspace))
  (gethash name (ensure-templates container)))

(defvar *connection*)
(defvar *uri* nil)
(defvar *version* nil)

(defmethod lsp:process-interface-method :around ((object    workspace)
                                                 (interface (eql :textdocument))
                                                 (method    t)
                                                 &key
                                                 text-document)
  (if (member method '(:didopen :didclose))
      (call-next-method)
      (let* ((*uri*     (assoc-value text-document :uri))
             (*version* (lsp:version (lsp:find-document *uri* object))))
        (call-next-method))))

(defmethod lsp:make-document ((container workspace)
                              (language  t)
                              (version   t)
                              (text      t))
  (let ((doc (make-instance (ecase language
                          (:template-recipe     'template-document)
                          (:project-recipe      'project-document)
                          (:distribution-recipe 'distribution-document))
                        :language  language
                        :version   version
                        :text      text
                        :workspace container)))
    (log:error doc)
    doc))
