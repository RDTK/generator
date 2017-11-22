;;;; workspace-methods.lisp --- TODO.
;;;;
;;;; Copyright (C) 2016, 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.language-server)

;;; Workspace-level events

(defmethod process-interface-method ((object    workspace)
                                     (interface (eql :workspace))
                                     (method    (eql :didchangeconfiguration))
                                     &key
                                       ))

(defmethod process-interface-method ((object    workspace)
                                     (interface (eql :workspace))
                                     (method    (eql :didchangewatchedfiles))
                                     &key
                                       ))

;;; Document-related methods

(defmethod process-interface-method ((object    workspace)
                                     (interface (eql :textdocument))
                                     (method    (eql :didopen))
                                     &key
                                     text-document)
  (let+ (((&values uri version) (parse-text-document text-document))
         (language-id (assoc-value text-document :language-id))
         (language    (make-keyword (string-upcase language-id)))
         (text        (assoc-value text-document :text)))
    (log:info "new document" uri version text)
    (setf (find-document uri object) (make-instance 'document
                                                    :language language
                                                    :version  version
                                                    :text     text))))

(defmethod process-interface-method ((object    workspace)
                                     (interface (eql :textdocument))
                                     (method    (eql :didclose))
                                     &key
                                       text-document)
  (let ((uri (parse-text-document text-document)))
    (setf (find-document uri object) nil)))

(defmethod process-interface-method ((object    workspace)
                                     (interface (eql :textdocument))
                                     (method    t)
                                     &rest args &key text-document)
  (let+ (((&values uri version) (parse-text-document text-document))
         (document (find-document uri object)))
    (apply #'process-method document method
           :version version (remove-from-plist args :text-document))))
