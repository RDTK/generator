;;;; document.lisp --- Document classes for different recipe types.
;;;;
;;;; Copyright (C) 2016, 2017, 2018, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:build-generator.language-server)

(defclass build-generator-document (lsp:document
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
               :initform nil)
   (%location->object :accessor location->object
                      :initform nil)))

(defmethod contrib:make-context-contributors ((document build-generator-document))
  (list (make-instance 'structure-context-contributor)
        (make-instance 'variable-name-context-contributor)
        (make-instance 'variable-value-context-contributor)))

(defmethod contrib:make-completion-contributors ((document build-generator-document))
  (list (make-instance 'structure-completion-contributor)
        (make-instance 'variable-name-completion-contributor)
        (make-instance 'variable-value-completion-contributor)))

(defmethod contrib:make-hover-contributors ((document build-generator-document))
  (list (make-instance 'variable-hover-contributor)))

(defmethod (setf lsp:text) :after ((new-value string)
                                   (document  build-generator-document))
  (let ((project::*projects*         (make-hash-table :test #'equal))
        (project::*locations*        (make-hash-table :test #'eq))
        (project::*location->object* (make-hash-table :test #'eq))

        (errors    '())
        (result    nil)
        (locations nil)
        (source    nil)
        (index     (make-instance 'text.source-location.lookup::range-index)))
    (handler-bind ((error (lambda (condition)
                            (log:warn "~@<Error during parsing:~:@_~A~@:>" condition)
                            (push condition errors)
                            (unless (typep condition '(or undefined-function
                                                          unbound-variable))
                              (continue))
                            (log:error "~@<Unrecoverable error during parsing:~:@_~A~@:>" condition))))
      (with-simple-restart (continue "Abort parse")
        (setf (values result locations source)
              (let ((pathname (pathname (file-namestring (pathname *uri*)))))
                (values
                 (parse document (lsp:text document) pathname index)
                 project::*locations*)))))
    (setf (object document)    result
          (locations document) locations
          (source document)    source
          (index document)     index
          (location->object document) project::*location->object*)
    (flet ((diagnostic (condition)
             (list (make-instance 'protocol.language-server.protocol:diagnostic
                                  :annotation (first (project::annotations condition))
                                  :message    condition))))
      (methods:publish-diagnostics document (mappend #'diagnostic errors)))))

(defmethod contrib:contexts :around ((workspace t) ; HACK))))
                                     (document  build-generator-document)
                                     (position  t))
  (reinitialize-instance position :column (max 0 (1- (sloc:column position))))
  (call-next-method))

(defmethod methods:definition ((workspace t)
                               (document  build-generator-document)
                               (position  t))
  (when-let* ((locations (lookup:lookup position (index document)))
              (name      (gethash (first locations) (location->object document)))
              (template  (project:find-template name :if-does-not-exist nil))
              (location (project::location-of template)))
    (log:error locations name template location )
    (list (proto:unparse-location location))))

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

(defmethod methods:code-actions ((workspace t)
                                 (document  build-generator-document)
                                 (range     t)
                                 (context   t))
  nil)

;;; `project-document'

(defclass project-document (build-generator-document)
  ())

(defmethod parse ((document project-document) (text string) (pathname t) (index t))
  (let ((project::*templates* (lparallel:force (ensure-templates (workspace document))))
        (project::*location-hook* ; TODO do this in superclass's method?
          (lambda (object location)
            (declare (ignore object))
            (text.source-location.lookup:add! location index))))
    (project::load-project-spec/yaml
     text
     :pathname          pathname
     :generator-version "0.25.0")))

(defmethod contrib:make-context-contributors ((document project-document))
  (list* (make-instance 'template-name-context-contributor)
         (call-next-method)))

(defmethod contrib:make-completion-contributors ((document project-document))
  (list* (make-instance 'template-name-completion-contributor)
         (call-next-method)))

;;; `distribution-document'

(defclass distribution-document (build-generator-document)
  ())

(defmethod parse ((document distribution-document)
                  (text     string)
                  (pathname t)
                  (index    t))
  (let* ((distribution (let ((project::*location-hook*
                               (lambda (object location)
                                 (declare (ignore object))
                                 (text.source-location.lookup:add! location index))))
                         (project::load-distribution/yaml
                          text
                          :pathname          pathname
                          :generator-version "0.25.0")))
         (project::*templates*        (lparallel:force (ensure-templates (workspace document))))
         (project::*projects*         (make-hash-table :test #'equal))
         (project::*locations*        (make-hash-table :test #'eq))
         (project::*location->object* (make-hash-table :test #'eq))
         (projects-files+versions (uiop:symbol-call '#:build-generator.commands '#:locate-projects
                                                    (list pathname) (list distribution)))

         (projects                (uiop:symbol-call '#:build-generator.commands '#:load-projects/versioned
                                                    projects-files+versions
                                                    :generator-version "0.25.0" ; generator-version
                                                    )))
    (lsp::debug1 (list :parse-distribution projects-files+versions projects))
    distribution))

(defmethod contrib:make-context-contributors ((document distribution-document))
  (list* (make-instance 'project-version-reference-context-contributor)
         (call-next-method)))

(defmethod contrib:make-completion-contributors ((document distribution-document))
  (list* (make-instance 'project-name-completion-contributor)
         (call-next-method)))

(defmethod contrib:make-hover-contributors ((document distribution-document))
  (list* (make-instance 'project-version-hover-contributor)
         (call-next-method)))

;;; `template-document'

(defclass template-document (build-generator-document)
  ())

(defmethod parse ((document template-document) (text string) (pathname t) (index t))
  (let ((name (pathname-name pathname))
        (project::*templates* (make-hash-table :test #'equal))
        (project::*location-hook*
          (lambda (object location)
            (declare (ignore object))
            (text.source-location.lookup:add! location index))))
    (project::loading-template (name)
      (project::load-one-template/yaml
       text
       :pathname          pathname
       :generator-version "0.25.0"))))

(defmethod contrib:make-context-contributors ((document template-document))
  (list* (make-instance 'template-name-context-contributor)
         (make-instance 'aspect-class-context-contributor)
         (call-next-method)))

(defmethod contrib:make-completion-contributors ((document template-document))
  (list* (make-instance 'template-name-completion-contributor)
         (make-instance 'aspect-class-completion-contributor)
         (call-next-method)))
