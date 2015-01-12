;;;; json.lisp --- Minimal JSON import for templates and projects.
;;;;
;;;; Copyright (C) 2012-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.project)

(defun check-generator-version (spec)
  (when-let ((required-version (cdr (assoc :minimum-generator-version spec))))
    (let ((provided-version (jenkins.project-system:version/string)))
      (unless (uiop:version-compatible-p provided-version required-version)
        (error "~@<The template requires generator version ~S, but ~
                this generator is version ~S.~@:>"
               required-version provided-version)))))

(defun check-keys (object &optional expected (exhaustive? t))
  (let+ ((seen     '())
         (expected (mapcar #'ensure-list expected))
         (extra    '())
         ((&flet invalid-keys (reason keys)
            (error "~@<~A key~P: ~{~A~^, ~}.~@:>"
                   reason (length keys) keys))))
    (loop :for (key . value) :in object :do
       (cond
         ((member key seen :test #'eq)
          (error "~@<Duplicate key: ~A.~@:>" key))
         ((member key expected :test #'eq :key #'car)
          (alexandria:removef expected key :test #'eq :key #'car))
         (t
          (push key extra)))
       (push key seen))
    (when-let ((missing (remove nil expected :key #'cdr)))
      (invalid-keys "Missing required" (mapcar #'car missing)))
    (when (and exhaustive? extra)
      (invalid-keys "Unexpected" extra)))
  object)

(defun process-variables (alist)
  (let ((entries (make-hash-table :test #'eq)))
    (loop :for (key . value) :in alist :do
             (when (starts-with-subseq "__" (string key))
               (error "~@<Variable name ~A starts with \"__\". These ~
                       variable names are reserved for internal ~
                       use.~@:>"
                      key))
             (push value (gethash key entries)))
    (loop :for key :being :the :hash-key :of entries
          :using (:hash-value value)
          :do (unless (length= 1 value)
                (error "~@<Multiple definitions of variable ~A: ~
                        ~{~S~^, ~}.~@:>"
                       key value))
       :collect (value-cons key (first value)))))

(defun load-template/json-1 (pathname)
  (let+ ((spec (%decode-json-from-source pathname))
         ((&flet lookup (name &optional (where spec))
            (cdr (assoc name where))))

         ((&flet make-aspect-spec (spec parent)
            (check-keys spec '((:name . t) (:aspect . t) :variables
                               :conditions))
            (make-instance 'aspect-spec
                           :name       (lookup :name spec)
                           :parent     parent
                           :aspect     (lookup :aspect spec)
                           :variables  (process-variables (lookup :variables spec))
                           :conditions (lookup :conditions spec))))

         ((&flet make-job-spec (spec parent)
            (check-keys spec '((:name . t) :variables :conditions))
            (make-instance 'job-spec
                           :name       (lookup :name spec)
                           :parent     parent
                           :variables  (process-variables (lookup :variables spec))
                           :conditions (lookup :conditions spec))))

         (name (lookup :name))
         (template (make-instance 'template)))
    (check-generator-version spec)
    (check-keys spec '(:minimum-generator-version
                       (:name . t) :inherit :variables :aspects :jobs))
    (setf (find-template name)
          (reinitialize-instance
           template
           :name      name
           :inherit   (mapcar #'find-template (lookup :inherit))
           :variables (process-variables (lookup :variables))
           :aspects   (mapcar (rcurry #'make-aspect-spec template) (lookup :aspects))
           :jobs      (mapcar (rcurry #'make-job-spec template) (lookup :jobs))))))

(defun load-template/json (pathname)
  (handler-bind ((error (lambda (condition)
                          (error "~@<Error when loading template from ~
                                  ~S: ~A~@:>"
                                 pathname condition))))
    (load-template/json-1 pathname)))

(defun load-project-spec/json-1 (pathname &key version-test)
  (let+ ((spec (%decode-json-from-source pathname))
         ((&flet lookup (name &optional (where spec))
            (cdr (assoc name where))))
         ((&flet make-version-spec (spec parent)
            (check-keys spec '((:name . t) :variables :catalog))
            (make-instance 'version-spec
                           :name      (lookup :name spec)
                           :parent    parent
                           :variables (value-acons
                                       :__catalog (lookup :catalog spec)
                                       (process-variables (lookup :variables spec))))))
         (name (lookup :name)))
    (check-generator-version spec)
    (check-keys spec '(:minimum-generator-version
                       (:name . t) (:templates . t) (:variables . t)
                       :versions :catalog))
    (unless (string= name (pathname-name pathname))
      (error "~@<Value of \"name\" attribute, ~S, does not match ~
              filename ~S.~@:>"
             name (pathname-name pathname)))
    (let ((instance (make-instance 'project-spec :name name)))
      (reinitialize-instance
       instance
       :templates (mapcar #'find-template (lookup :templates))
       :variables (value-acons
                   :__catalog (lookup :catalog)
                   (process-variables (lookup :variables)))
       :versions  (mapcar (rcurry #'make-version-spec instance)
                          (if version-test
                              (remove-if (lambda (version)
                                           (let ((name (lookup :name version)))
                                             (not (funcall version-test name))))
                                         (lookup :versions))
                              (lookup :versions)))))))

(defun load-project-spec/json (pathname &key version-test)
  (handler-bind ((error (lambda (condition)
                          (error "~@<Error when loading project ~
                                  description from ~S: ~A~@:>"
                                  pathname condition))))
    (load-project-spec/json-1 pathname :version-test version-test)))

(defun load-distribution-spec/json-1 (pathname)
  (let+ ((spec (%decode-json-from-source pathname))
         ((&flet lookup (name &optional (where spec))
            (cdr (assoc name where))))
         (name (lookup :name))
         (projects-seen (make-hash-table :test #'equal))
         ((&flet check-version (version)
            (cond
              ((not (and (typep version '(cons string (cons string list)))
                         (every #'stringp (nthcdr 2 version))))
                (cerror "~@<Continue without the project entry~@:>"
                        "~@<Project entry~
                         ~@:_~@:_~
                         ~2@T~A~
                         ~@:_~@:_~
                         is not a project name followed by one or more ~
                         project versions.~:@>"
                        (json:encode-json-to-string version)))
              ((when-let ((previous (gethash (first version) projects-seen)))
                 (cerror "~@<Ignore the additional project entry~@:>"
                         "~@<Project entry~
                          ~@:_~@:_~
                          ~2@T~A~
                          ~@:_~@:_~
                          followed by another entry~
                          ~@:_~@:_~
                          ~2@T~A~
                          ~@:_~@:_~
                          for same project. Multiple project versions ~
                          have to be described in a single entry.~@:>"
                         (json:encode-json-to-string previous)
                         (json:encode-json-to-string version))))
              (t
               (setf (gethash (first version) projects-seen) version)
               (list version))))))
    (check-generator-version spec)
    (check-keys spec '(:minimum-generator-version
                       (:name . t) :variables (:versions . t)
                       :catalog))
    (unless (string= name (pathname-name pathname))
      (error "~@<Value of \"name\" attribute, ~S, does not match ~
              filename ~S.~@:>"
             name (pathname-name pathname)))
    (make-instance 'distribution-spec
                   :name      name
                   :variables (value-acons :__catalog (lookup :catalog)
                                           (process-variables (lookup :variables)))
                   :versions  (mapcan #'check-version (lookup :versions)))))

(defun load-distribution/json (pathname)
  (handler-bind ((error (lambda (condition)
                          (error "~@<Error when loading distribution ~
                                  description from ~S: ~A~@:>"
                                 pathname condition))))
    (load-distribution-spec/json-1 pathname)))

(defun %decode-json-from-source (source)
  (let ((json::*json-identifier-name-to-lisp* #'string-upcase))
    (json:decode-json-from-source source)))
