;;;; document-methods.lisp --- TODO.
;;;;
;;;; Copyright (C) 2016, 2017, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.language-server)

;;;

(defclass workspace (lsp:standard-workspace)
  ())

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

(defclass build-generator-document (lsp:document)
  ((object    :accessor object
              :initform nil)
   (locations :accessor locations
              :initform nil)))

(defmethod lsp:make-document ((container workspace)
                              (language  t)
                              (version   t)
                              (text      t))
  (log:info "creating template document")
  (make-instance (ecase language
                   (:template-recipe     'template-document)
                   (:project-recipe      'project-document)
                   (:distribution-recipe 'distribution-document))
                 :language language
                 :version  version
                 :text     text))

(defclass project-document (build-generator-document)
  ())

(defmethod (setf lsp:text) :after ((new-value string)
                                   (document  project-document))
  (let ((errors    '())
        (result    nil)
        (locations nil))
    (handler-bind ((error (lambda (condition)
                            (log:warn "~@<Error during parsing:~:@_~A~@:>" condition)
                            (push condition errors)
                            (unless (typep condition '(or undefined-function
                                                          unbound-variable))
                              (continue))
                            (log:error "~@<Unrecoverable error during parsing:~:@_~A~@:>" condition))))
      (clrhash jenkins.model.project::*projects*)
      (clrhash jenkins.model.project::*locations*)
      (with-simple-restart (continue "Abort parse")
        (setf (values result locations)
              (let ((pathname (pathname (file-namestring (pathname *uri*)))))
                (values
                 (jenkins.model.project:load-project-spec/json-or-yaml
                  (make-string-input-stream (lsp:text document))
                  :pathname          pathname
                  :content           (lsp:text document)
                  :generator-version "0.20.0")
                 jenkins.model.project::*locations*)))))
    (setf (object document)    result
          (locations document) locations)
    (when locations
      (log:info (hash-table-alist locations)))
    (flet ((diagnostic (condition)
             (make-instance 'protocol.language-server.protocol:diagnostic
                            :annotation (first (jenkins.model.project::annotations condition))
                            :message    condition)))
      (methods::publish-diagnostics document (map 'list #'diagnostic errors)))))

(defclass distribution-document (build-generator-document)
  ())

(defclass template-document (build-generator-document)
  ())

(defmethod (setf lsp:text) :after ((new-value string)
                                   (document  template-document))
  (let ((errors    '())
        (result    nil)
        (locations nil))
    (handler-bind ((error (lambda (condition)
                            (log:warn "~@<Error during parsing:~:@_~A~@:>" condition)
                            (push condition errors)
                            (unless (typep condition '(or undefined-function
                                                          unbound-variable))
                              (continue))
                            (log:error "~@<Unrecoverable error during parsing:~:@_~A~@:>" condition))))
      (clrhash jenkins.model.project::*templates*)
      (clrhash jenkins.model.project::*locations*)
      (with-simple-restart (continue "Abort parse")
        (setf (values result locations)
              (let* ((pathname (pathname (file-namestring (pathname *uri*))))
                     (name     (pathname-name pathname)))
                (values
                 (jenkins.model.project::loading-template (name)
                   (jenkins.model.project::load-one-template/json-or-yaml
                    (make-string-input-stream (lsp:text document))
                    :pathname          pathname
                    :content           (lsp:text document)
                    :generator-version "0.20.0"))
                 jenkins.model.project::*locations*)))))
    (log:info "still alive 0")
    (setf (object document)    result
          (locations document) locations)
    (log:info "still alive 1")
    (when locations
      (log:info (hash-table-alist locations)))
    (log:info "still alive 2")
    (flet ((diagnostic (condition)
             (make-instance 'protocol.language-server.protocol:diagnostic
                            :annotation (first (jenkins.model.project::annotations condition))
                            :message    condition)))
      (methods::publish-diagnostics document (map 'list #'diagnostic errors)))))

;;;

(defmethod methods:definition ((workspace t)
                               (document  build-generator-document)
                               (position  t))
  nil)

(defmethod methods:highlight-in-document ((workspace t)
                                          (document  build-generator-document)
                                          (version   t)
                                          (position  t))
  (or (let+ ((locations (locations document)))
        (when locations
          (when-let ((location (find-if (curry #'text.source-location.lookup:location-in? position)
                                        (hash-table-alist locations) :key #'cdr :from-end t)))
            (vector (proto:make-highlight :text (text.source-location:range (cdr location)))))))
      #()))

(defmethod methods:hover ((workspace t)
                          (document  build-generator-document)
                          (position  t))
  (let+ ((locations (locations document)))
    (when locations
      (when-let ((location (find-if (curry #'text.source-location.lookup:location-in? position)
                                    (hash-table-alist locations) :key #'cdr :from-end t)))
        (log:info (car location))
        (when (typep (car location) '(cons keyword))
          (when-let ((variable (jenkins.model.variables:find-variable (car (car location)))))
           (proto:make-hover-result
            (jenkins.model.variables:variable-info-documentation variable)
            :range (text.source-location:range (cdr location)) )))))))

(defmethod methods:completion ((workspace t)
                               (document  build-generator-document)
                               (position  t))
  ; (complete :variable-name word)
  #+no (complete :variable-value)
  nil)

(defmethod methods:code-actions ((workspace t)
                                 (document  build-generator-document)
                                 (range     t)
                                 (context   t))
  nil)
