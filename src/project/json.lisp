;;;; json.lisp --- Minimal JSON import for templates and projects.
;;;;
;;;; Copyright (C) 2012, 2013, 2014, 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.project)

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
          :collect key :collect (first value))))

(defun load-template/json-1 (pathname)
  (let+ ((spec (%decode-json-from-source pathname))
         ((&flet lookup (name &optional (where spec))
            (cdr (assoc name where))))

         ((&flet make-aspect-spec (spec parent)
            (make-instance 'aspect-spec
                           :name       (lookup :name spec)
                           :parent     parent
                           :aspect     (lookup :aspect spec)
                           :filter     (lookup :filter spec) ; TODO remove?
                           :variables  (process-variables (lookup :variables spec))
                           :conditions (lookup :conditions spec))))

         ((&flet make-job-spec (spec parent)
            (make-instance 'job-spec
                           :name       (lookup :name spec)
                           :parent     parent
                           :tags       (lookup :tags spec)
                           :variables  (process-variables (lookup :variables spec))
                           :conditions (lookup :conditions spec))))

         (name (lookup :name))
         (template (make-instance 'template)))
    (when-let ((required-version (lookup :minimum-generator-version)))
      (let ((provided-version (jenkins.project-system:version/string)))
        (unless (uiop:version-compatible-p provided-version required-version)
          (error "~@<The template requires generator version ~S, but ~
                  this generator is version ~S.~@:>"
                 required-version provided-version))))
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
            (make-instance 'version-spec
                           :name      (lookup :name spec)
                           :parent    parent
                           :variables (list* :__catalog (lookup :catalog spec)
                                             (process-variables (lookup :variables spec))))))
         ((&flet make-job-spec (spec parent)
            (make-instance 'job-spec
                           :name       (lookup :name spec)
                           :parent     parent
                           :tags       (lookup :tags spec)
                           :conditions (lookup :conditions spec))))
         (name (lookup :name)))
    (unless (string= name (pathname-name pathname))
      (error "~@<Value of \"name\" attribute, ~S, does not match ~
              filename ~S.~@:>"
             name (pathname-name pathname)))
    (let ((instance (make-instance 'project-spec :name name)))
      (reinitialize-instance
       instance
       :templates (mapcar #'find-template (lookup :templates))
       :variables (list* :__catalog (lookup :catalog)
                         (process-variables (lookup :variables)))
       :versions  (mapcar (rcurry #'make-version-spec instance)
                          (if version-test
                              (remove-if (lambda (version)
                                           (let ((name (lookup :name version)))
                                             (not (funcall version-test name))))
                                         (lookup :versions))
                              (lookup :versions)))
       :jobs      (mapcar (rcurry #'make-job-spec instance)
                          (lookup :jobs))))))

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
         ((&flet check-version (version)
            (if (and (typep version '(cons string (cons string list)))
                     (every #'stringp (nthcdr 2 version)))
                (list version)
                (cerror "~@<Continue without the project entry~@:>"
                        "~@<Project entry [ ~{~S~^, ~} ] is not a project ~
                         name followed by one or more project versions.~:@>"
                        version)))))
    (unless (string= name (pathname-name pathname))
      (error "~@<Value of \"name\" attribute, ~S, does not match ~
              filename ~S.~@:>"
             name (pathname-name pathname)))
    (make-instance 'distribution-spec
                   :name      name
                   :variables (list* :__catalog (lookup :catalog)
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
