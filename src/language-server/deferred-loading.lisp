;;;; deferred-loading.lisp --- Deferred loading of recipes.
;;;;
;;;; Copyright (C) 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:build-generator.language-server)

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

;;; Recipe loading

(defclass deferred-recipes-mixin ()
  ((%workspace :initarg :workspace
               :reader  workspace)))

(defun map-recipes (function kind container)
  (let* ((repository (repository (workspace container)))
         (files      (project:recipe-truenames repository kind (if (eq kind :distribution) #P"**/*" :wild)))) ; TODO hack
    (log:error "Background-loading ~:D ~(~A~)~:*~:P~* from ~A"
               (length files) kind repository)
    (handler-bind (((and error util:continuable-error)
                     (lambda (condition)
                       (log:error "Error background-loading ~(~A~): ~A"
                                  kind condition)
                       (invoke-restart
                        (util:find-continue-restart condition)))))
      (map nil (lambda (filename)
                 (with-simple-restart (continue "Skip project recipe ~A"
                                                filename)
                   (funcall function filename repository)))
           files))))

;;; Templates

(defclass deferred-templates (deferred-collection
                              deferred-recipes-mixin)
  ())

(defmethod load-elements ((container deferred-templates))
  (let ((project::*templates* (make-hash-table :test #'equal))
         (project::*templates-lock* (bt:make-lock)))
    (map-recipes (lambda (filename repository)
                   (project:load-template/yaml
                    filename :repository repository))
                 :template container)
    project::*templates*))

;;; Projects

(defclass deferred-projects (deferred-collection
                             deferred-recipes-mixin)
  ())

(defmethod load-elements ((container deferred-projects))
  (let* ((workspace                (workspace container))
         (project::*templates*     (templates/table workspace :if-unavailable :block))
         (project::*projects*      nil)
         (projects                 (make-hash-table :test #'equal))
         (project::*projects-lock* (bt:make-lock)))
    (map-recipes
     (lambda (filename repository)
       (let ((project (project:load-project-spec/yaml
                       filename :repository        repository
                       :generator-version "0.30.0"
                       :version-test      (lambda (name pattern)
                                            (declare (ignore name pattern))
                                            '()))))
         (setf (gethash (model:name project) projects) project)))
     :project container)
    projects))

;;; Distributions

(defclass deferred-distributions (deferred-collection
                                  deferred-recipes-mixin)
  ())

(defmethod load-elements ((container deferred-distributions))
  (let* ((workspace                (workspace container))
         (project::*templates*     (templates/table workspace :if-unavailable :block))
         (project::*projects*      nil)
         (distributions            (make-hash-table :test #'equal))
         (project::*projects-lock* (bt:make-lock))
         ; (project::*distributions-lock* (bt:make-lock))
         )
    (map-recipes
     (lambda (filename repository)
       (let ((distribution (project:load-distribution/yaml
                            filename :repository        repository
                            :generator-version "0.30.0")))
         (setf (gethash (model:name distribution) distributions) distribution)))
     :distribution container)
    distributions))

;;; Persons

(defclass deferred-persons (deferred-collection
                            deferred-recipes-mixin)
  ())

(defmethod load-elements ((container deferred-persons))
  (let* ((workspace               (workspace container))
         (project::*persons-lock* (bt:make-lock))
         (persons                 (make-hash-table :test #'equal)))
    (map-recipes (lambda (filename repository)
                   (let ((person (project:load-person/yaml
                                  filename :repository        repository
                                           :generator-version "0.28.0")))
                     (setf (gethash (rosetta.model:name person) persons) person)))
                 :person container)
    persons))

;;; Analysis results

(defclass deferred-analysis (deferred-collection)
  ((%object :initarg :object
            :accessor object)
   (%key    :accessor %key
            :initform nil)))

(defmethod (setf object) :after ((new-value t) (object deferred-analysis))
  (let* ((project-version (object object))
         (repository      (handler-case
                              (var:value project-version :repository    nil)
                            (var:undefined-variable-error () nil)))
         (sub-directory   (handler-case
                              (var:value project-version :sub-directory nil)
                            (var:undefined-variable-error () nil)))
         (natures         (handler-case
                              (var:value project-version :natures       '())
                            (var:undefined-variable-error () nil)))
         (branches        (handler-case
                              (var:value project-version :branches      '())
                            (var:undefined-variable-error () nil)))
         (new-key         (list repository branches sub-directory natures))
         (old-key         (%key object)))
    (when (not (equal old-key new-key))
      (setf (%key object)      new-key
            (%elements object) nil))))

(defmethod load-elements ((container deferred-analysis))
  (let+ (((&accessors-r/o ((repository branches sub-directory natures) %key))
          container)
         (natures  (mappend (lambda (name)
                              (when-let ((symbol (intern (string-upcase name)
                                                         '#:keyword)))
                                (list symbol)))
                            natures))
         (versions (loop :for branch :in branches
                         :collect (list :branch        branch
                                        :sub-directory (when sub-directory
                                                         (uiop:ensure-directory-pathname sub-directory))
                                        :natures       natures)))
         (errors   '())
         (results  (handler-case
                       (handler-bind (((and error util:continuable-error)
                                        (lambda (condition)
                                          (when-let ((restart (util:find-continue-restart condition)))
                                            (push condition errors)
                                            (invoke-restart restart)))))
                         (build-generator.analysis:analyze
                          repository :auto :versions versions))
                     (error (condition)
                       condition))))
    (let ((table (make-hash-table :test #'equal)))
      (loop :for version :in versions
            :for branch  =   (getf version :branch)
            :for result  :in results
            :do (setf (gethash branch table)
                      (typecase results
                        (condition results)
                        (cons      (list result branch natures))
                        (t         (first errors)))))
      table)))
