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
  ((%repository        :initarg  :repository
                       :reader   repository
                       :writer   (setf %repository))
   ;; Documents by kind
   (%project-documents :accessor project-documents
                       :initform '())
   ;;
   (%templates         :accessor %templates
                       :initform nil)
   (%projects          :accessor %projects
                       :initform nil)
   (%persons           :accessor %persons
                       :initform nil)
   ;; Platform
   (%platform-packages :accessor %platform-packages
                       :initform nil)))

(defmethod shared-initialize :after ((instance   workspace)
                                     (slot-names t)
                                     &key
                                     (root-uri   nil root-uri-supplied?)
                                     (root-path  nil root-path-supplied?)
                                     (repository nil repository-supplied?))
  (declare (ignore root-uri root-path repository))
  (when (and (or root-uri-supplied? root-path-supplied?)
             (not repository-supplied?))
    (setf (%repository instance) (project:make-populated-recipe-repository
                                  (lsp:root-directory instance) "toolkit"))))



;;; Templates

(defmethod load-templates ((container workspace))
  (let* ((project::*templates* (make-hash-table :test #'equal))
         (project::*templates-lock* (bt:make-lock))
         (repository (repository container))
         (pattern    (project:recipe-path repository :template :wild)))
    (log:error "Background-loading templates from ~A" pattern)
    (mappend (lambda (filename)
               (with-simple-restart (continue "Skip")
                 (list (project:load-template/yaml
                        filename :repository repository))))
             (directory pattern))
    project::*templates*))

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

;;; Projects

(defmethod load-projects ((container workspace))
  (let* ((project::*templates* (lparallel:force (ensure-templates container)))
         (project::*projects* nil ; (make-hash-table :test #'equal)
                                            )
         (project::*projects-lock* (bt:make-lock))
         (repository (repository container))
         (pattern    (project:recipe-path repository :project :wild)))
    (handler-bind (((and error jenkins.util:continuable-error)
                     (compose #'invoke-restart #'jenkins.util:find-continue-restart)))
      (mappend (lambda (filename)
                 (with-simple-restart (continue "Skip")
                   (list (project:load-project-spec/yaml
                          filename :repository repository))))
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
  ;; TODO merge with (map 'list #'object <project-documents>)
  (ensure-projects container))

;;; Platform packages

(defmethod load-platform-packages ((container workspace))
  (jenkins.analysis:installed-packages))

(defmethod ensure-platform-packages ((container workspace))
  (loop :for platform-packages = (%platform-packages container)
        :when platform-packages :do (return platform-packages)
        :do (let ((promise (lparallel:promise)))
              (when (null (sb-ext:compare-and-swap
                           (slot-value container '%platform-packages) nil promise))
                                        ; (methods::log-message (proto::make-message :info "Background-loading platform-packages"))
                (lparallel:future (lparallel:fulfill promise
                                    (load-platform-packages container)))))))

;;;

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

#+todo-later (defmethod lsp:note-adopted progn ((container workspace)
                                   (document  project-document)) ; TODO removal
  (push document (project-documents container)))
