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
   (%templates         :accessor %templates)
   (%projects          :accessor %projects)
   (%distributions     :accessor %distributions)
   (%persons           :accessor %persons)
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
                                  (lsp:root-directory instance) "toolkit")))

  (setf (%templates instance)     (make-instance 'deferred-templates     :workspace instance)
        (%projects instance)      (make-instance 'deferred-projects      :workspace instance)
        (%distributions instance) (make-instance 'deferred-distributions :workspace instance)
        (%persons instance)       (make-instance 'deferred-persons       :workspace instance)))

;;; Templates

(defmethod templates/table ((container workspace) &key if-unavailable)
  (table (%templates container) :if-unavailable if-unavailable))

(defmethod templates ((container workspace) &key if-unavailable)
  (elements (%templates container) :if-unavailable if-unavailable))

(defmethod find-template ((name t) (container workspace) &key if-unavailable)
  (find-element name (%templates container) :if-unavailable if-unavailable))

;;; Projects

(defmethod projects ((container workspace) &key if-unavailable)
  (elements (%projects container) :if-unavailable if-unavailable))

(defmethod find-project ((name t) (container workspace) &key if-unavailable)
  (find-element name (%projects container) :if-unavailable if-unavailable))

;;; Distributions

(defmethod distributions ((container workspace) &key if-unavailable)
  (elements (%distributions container) :if-unavailable if-unavailable))

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
