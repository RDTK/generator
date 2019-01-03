;;;; document.lisp --- Document classes for different recipe types.
;;;;
;;;; Copyright (C) 2016, 2017, 2018, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.language-server)

(defclass build-generator-document (lsp:document
                                    contrib:diagnostics-contributors-mixin
                                    contrib:context-contributors-mixin
                                    contrib:completion-contributors-mixin
                                    contrib:hover-contributors-mixin)
  ((%workspace :initarg  :workspace
               :reader   workspace) ; TODO do this in protocol.language-server?
   (%object    :accessor object
               :initform nil)
   (%locations :accessor locations
               :initform nil)
   (%source    :accessor source
               :initform nil)
   (%index     :accessor index
               :initform nil)))

(defmethod contrib:make-contributors ((document build-generator-document)
                                      (aspect   (eql 'contrib::diagnostics)))
  (list (make-instance 'tabs-diagnostics-contributor)
        (make-instance 'variable-diagnostics-contributor)))

(defmethod contrib:make-contributors ((document build-generator-document)
                                      (aspect   (eql 'contrib::context)))
  (list (make-instance 'structure-context-contributor)
        (make-instance 'variable-name-context-contributor)
        (make-instance 'variable-value-context-contributor)))

(defmethod contrib:make-contributors ((document build-generator-document)
                                      (aspect   (eql 'contrib::completion)))
  (list (make-instance 'structure-completion-contributor)
        (make-instance 'variable-name-completion-contributor)
        (make-instance 'variable-value-completion-contributor)))

(defmethod contrib:make-contributors ((document build-generator-document)
                                      (aspect   (eql 'contrib::hover)))
  (list (make-instance 'variable-hover-contributor)))

(defmethod (setf lsp:text) :after ((new-value string)
                                   (document  build-generator-document))
  (unless (starts-with-subseq "file:///" *uri*)
    (error "unsupported URI: ~S" *uri*))

  (let* ((pathname (pathname (subseq *uri* (length "file://"))))
         (source   (setf (source document)
                         (sloc:make-source pathname :content (lsp:text document))))

         (errors    '())
         (result    nil)
         (locations nil)
         (source    nil)
         (index     (make-instance 'text.source-location.lookup::range-index))

         (project::*locations* (make-instance 'project::locations
                                                            :hook (lambda (object location)
                                                                    (declare (ignore object))
                                                                    (text.source-location.lookup:add! location index)))))
    (handler-bind ((error (lambda (condition)
                            (log:warn "~@<Error during parsing:~:@_~A~@:>" condition)
                            (push condition errors)
                            (unless (typep condition '(or undefined-function
                                                          unbound-variable))
                              (continue))
                            (log:error "~@<Unrecoverable error during parsing:~:@_~A~@:>" condition))))
      (with-simple-restart (continue "Abort parse")

        (setf result (parse document (lsp:text document) pathname))))
    (setf (object document)    result
          (locations document) project::*locations*
          (index document)     index)
    (let ((diagnostics (contrib:diagnostics (workspace document) document))
          (messages    '()))
      (flet ((do-condition (condition)
               (if (compute-applicable-methods #'project::annotations (list condition))
                   (push (make-instance 'protocol.language-server.protocol:diagnostic
                                        :annotation (first (project::annotations condition))
                                        :message    condition)
                         diagnostics)
                   (push (proto:make-message :error (princ-to-string condition))
                         messages))))
        (map nil #'do-condition errors)
        (methods:publish-diagnostics document diagnostics)
        (map nil #'methods::log-message messages)))))

(defmethod contrib:contexts :around ((workspace t) ; HACK))))
                                     (document  build-generator-document)
                                     (position  t))
  (reinitialize-instance position :column (max 0 (1- (sloc:column position))))
  (call-next-method))

(defmethod methods:definition ((workspace t)
                               (document  build-generator-document)
                               (position  t))
  (when-let* ((locations (lookup:lookup position (index document)))
              (name      (project::object-at (first locations) (locations document)))
              (template  (project:find-template name :if-does-not-exist nil))
              (location  (project::location-of template (locations document))))
    (list (proto:unparse location))))

(defmethod methods:highlight-in-document ((workspace t)
                                          (document  build-generator-document)
                                          (version   t)
                                          (position  t))
  (or (when-let ((locations (lookup:lookup position (index document))))
        (vector (proto:make-highlight :text (text.source-location:range (first locations)))))
      #()))

(defmethod methods:code-actions ((workspace t)
                                 (document  build-generator-document)
                                 (range     t)
                                 (context   t))
  nil)

;;; `project-document'

(defclass project-document (build-generator-document)
  ())

(defmethod parse ((document project-document) (text string) (pathname t))
  (let ((project::*templates* (lparallel:force (ensure-templates (workspace document)))))
    (project::load-project-spec/yaml
     text
     :pathname          pathname
     :generator-version "0.26.0"
     :version-test      (lambda (name pattern)
                          (cond (name
                                 t)
                                (pattern
                                 (remove pattern '("foo")
                                         :test-not #'ppcre:scan)))))))

(defmethod contrib:make-contributors ((document project-document)
                                      (aspect   (eql 'contrib::context)))
  (list* (make-instance 'template-name-context-contributor)
         (call-next-method)))

(defmethod contrib:make-contributors ((document project-document)
                                      (aspect   (eql 'contrib::completion)))
  (list* (make-instance 'template-name-completion-contributor)
         (call-next-method)))

;;; `distribution-document'

(defclass distribution-document (build-generator-document)
  ())

(defmethod parse ((document distribution-document) (text string) (pathname t))
  (let* ((distribution (project::load-distribution/yaml
                        text
                        :pathname          pathname
                        :generator-version "0.26.0"))
         (projects-files+versions (uiop:symbol-call '#:jenkins.project.commands '#:locate-projects
                                                    (list pathname) (list distribution)))
         (project::*templates*        (lparallel:force (ensure-templates (workspace document))))
         (project::*projects*         (make-hash-table :test #'equal))
         (project::*locations*        (make-instance 'project::locations
                                                                   :hook nil))
         (projects (uiop:symbol-call '#:jenkins.project.commands '#:load-projects/versioned
                                     projects-files+versions
                                     :generator-version "0.26.0" ; generator-version
                                     )))
    distribution))

(defmethod contrib:make-contributors ((document distribution-document)
                                      (aspect   (eql 'contrib::context)))
  (list* (make-instance 'project-version-reference-context-contributor)
         (call-next-method)))

(defmethod contrib:make-contributors ((document distribution-document)
                                      (aspect   (eql 'contrib::completion)))
  (list* (make-instance 'project-name-completion-contributor)
         (call-next-method)))

(defmethod contrib:make-contributors ((document distribution-document)
                                      (aspect   (eql 'contrib::hover)))
  (list* (make-instance 'project-version-hover-contributor)
         (call-next-method)))

;;; `template-document'

(defclass template-document (build-generator-document)
  ())

(defmethod parse ((document template-document) (text string) (pathname t))
  (let ((name (pathname-name pathname))
        (project::*templates* (make-hash-table :test #'equal)))
    (project::loading-template (name)
      (project::load-one-template/yaml
       text
       :pathname          pathname
       :generator-version "0.26.0"))))

(defmethod contrib:make-contributors ((document template-document)
                                      (aspect   (eql 'contrib::context)))
  (list* (make-instance 'template-name-context-contributor)
         (make-instance 'aspect-class-context-contributor)
         (call-next-method)))

(defmethod contrib:make-contributors ((document template-document)
                                      (aspect   (eql 'contrib::completion)))
  (list* (make-instance 'template-name-completion-contributor)
         (make-instance 'aspect-class-completion-contributor)
         (call-next-method)))
