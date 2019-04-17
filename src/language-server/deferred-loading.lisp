(cl:in-package #:jenkins.language-server)

(defclass deferred-collection ()
  ((%elements :accessor %elements
              :initform nil)))

(defmethod ensure-elements ((container deferred-collection))
  (loop :for elements = (%elements container)
        :when elements :do (return elements)
        :do (let ((promise (lparallel:promise)))
              (when (null (sb-ext:compare-and-swap
                           (slot-value container '%elements) nil promise))
                ;; (methods::log-message (proto:make-message :info "Background-loading templates"))
                (lparallel:future (lparallel:fulfill promise
                                    (load-elements container)))))))

(defmethod table ((container deferred-collection) &key if-unavailable)
  (let ((elements (ensure-elements container)))
    (cond ((or (lparallel:fulfilledp elements) (eq if-unavailable :block))
           (lparallel:force elements))
          ((eq if-unavailable :promise)
           (lparallel:future (lparallel:force elements))))))

(defmethod elements ((container deferred-collection) &key if-unavailable)
  (let ((elements (ensure-elements container)))
    (cond ((or (lparallel:fulfilledp elements) (eq if-unavailable :block))
           (hash-table-values (lparallel:force elements)))
          ((eq if-unavailable :promise)
           (lparallel:future (hash-table-values (lparallel:force elements)))))))

(defmethod find-element ((name t) (container deferred-collection)
                         &key if-unavailable)
  ;; TODO merge with (map 'list #'object <project-documents>)
  (let ((elements (ensure-elements container)))
    (cond ((or (lparallel:fulfilledp elements) (eq if-unavailable :block))
           (gethash name (lparallel:force elements)))
          ((eq if-unavailable :promise)
           (lparallel:future (gethash name (lparallel:force elements)))))))

;;; Templates

(defclass deferred-templates (deferred-collection)
  ((%workspace :initarg :workspace
               :reader  workspace)))

(defmethod load-elements ((container deferred-templates))
  (let* ((project::*templates* (make-hash-table :test #'equal))
         (project::*templates-lock* (bt:make-lock))
         (repository (repository (workspace container)))
         (pattern    (project:recipe-path repository :template :wild)))
    (log:error "Background-loading templates from ~A" pattern)
    (mappend (lambda (filename)
               (with-simple-restart (continue "Skip")
                 (list (project:load-template/yaml
                        filename :repository repository))))
             (directory pattern))
    project::*templates*))

;;; Projects

(defclass deferred-projects (deferred-collection)
  ((%workspace :initarg :workspace
               :reader  workspace)))

(defmethod load-elements ((container deferred-projects))
  (let* ((workspace                (workspace container))
         (project::*templates*     (templates workspace :if-unavailable :block))
         (project::*projects*      nil)
         (projects                 (make-hash-table :test #'equal))
         (project::*projects-lock* (bt:make-lock))
         (repository               (repository workspace))
         (pattern                  (project:recipe-path repository :project :wild)))
    (log:error "Background-loading projects from ~A" pattern)
    (handler-bind (((and error jenkins.util:continuable-error)
                     (compose #'invoke-restart #'jenkins.util:find-continue-restart)))
      (map nil (lambda (filename)
                 (with-simple-restart (continue "Skip")
                   (let ((project (project:load-project-spec/yaml
                                   filename :repository repository)))
                     (setf (gethash (model:name project) projects) project))))
           (directory pattern)))
    projects))

;;; Distributions

(defclass deferred-distributions (deferred-collection)
  ((%workspace :initarg :workspace
               :reader  workspace)))

(defmethod load-elements ((container deferred-distributions))
  (let* ((workspace                (workspace container))
         (project::*templates*     (templates workspace :if-unavailable :block))
         (project::*projects*      nil)
         (distributions            (make-hash-table :test #'equal))
         (project::*projects-lock* (bt:make-lock))
         ; (project::*distributions-lock* (bt:make-lock))
         (repository               (repository workspace))
         (pattern                  (project:recipe-path repository :distribution :wild)))
    (log:error "Background-loading distributions from ~A" pattern)
    (handler-bind (((and error jenkins.util:continuable-error)
                     (compose #'invoke-restart #'jenkins.util:find-continue-restart)))
      (map nil (lambda (filename)
                 (with-simple-restart (continue "Skip")
                   (let ((distribution (project:load-distribution/yaml
                                        filename :repository repository)))
                     (setf (gethash (model:name distribution) distributions) distribution))))
           (directory pattern)))
    distributions))

;;; Persons

(defclass deferred-persons (deferred-collection)
  ((%workspace :initarg :workspace
               :reader  workspace)))

(defmethod load-elements ((container deferred-persons))
  (let* ((workspace               (workspace container))
         (project::*persons-lock* (bt:make-lock))
         (persons                 (make-hash-table :test #'equal))
         (repository              (repository workspace))
         (pattern                 (project:recipe-path repository :person :wild)))
    (log:error "Background-loading persons from ~A" pattern)
    (handler-bind (((and error jenkins.util:continuable-error)
                     (lambda (c) (log:error "~A" c) (funcall  (compose #'invoke-restart #'jenkins.util:find-continue-restart) c))))
      (map nil (lambda (filename)
                 (with-simple-restart (continue "Skip")
                   (let ((person (project:load-person/yaml
                                  filename :repository        repository
                                           :generator-version "0.28.0")))
                     (log:error person)
                     (setf (gethash (rosetta.model:name person) persons) person))))
           (directory pattern)))
    persons))
