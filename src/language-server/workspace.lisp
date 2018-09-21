;;;; workspace.lisp --- TODO.
;;;;
;;;; Copyright (C) 2016, 2017, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.language-server)

(defclass workspace (lsp:standard-workspace)
  ((%templates :accessor %templates
               :initform nil)))

(defmethod load-templates ((container workspace))
  (let ((jenkins.model.project::*templates* (make-hash-table :test #'equal))
        (jenkins.model.project::*templates-lock* (bt:make-lock))
        (pattern (merge-pathnames "../templates/toolkit/*.template"
                                  (lsp:root-path container))))
    (log:error "Background-loading templates from ~A" pattern)
    (methods::log-message "Background-loading templates") ; log-message doesn't work from this thread
    (map nil #'jenkins.model.project:load-template/yaml (directory pattern))
    (methods::log-message "Done background-loading templates")
    jenkins.model.project::*templates*))

(defmethod ensure-templates ((container workspace))
  (loop :for templates = (%templates container)
        :when templates :do (return (lparallel:force templates))
        :do (let ((promise (lparallel:promise)))
              (when (null (sb-ext:compare-and-swap
                           (slot-value container '%templates) nil promise))
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
