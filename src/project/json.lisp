;;;; json.lisp --- Minimal JSON import for templates and projects.
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.project)

(defun load-template/json-1 (pathname)
  (let+ ((spec (json:decode-json-from-source pathname))
         ((&flet lookup (name &optional (where spec))
            (cdr (assoc name where))))

         ((&flet make-aspect-spec (spec parent)
            (make-instance 'aspect-spec
                           :name       (lookup :name spec)
                           :parent     parent
                           :aspect     (lookup :aspect spec)
                           :filter     (lookup :filter spec) ; TODO remove?
                           :variables  (alist-plist (lookup :variables spec))
                           :conditions (lookup :conditions spec))))

         ((&flet make-job-spec (spec parent)
            (make-instance 'job-spec
                           :name       (lookup :name spec)
                           :parent     parent
                           :tags       (lookup :tags spec)
                           :variables  (alist-plist (lookup :variables spec))
                           :conditions (lookup :conditions spec))))

         (name (lookup :name))
         (template (make-instance 'template)))
    (setf (find-template name)
          (reinitialize-instance
           template
           :name      name
           :inherit   (mapcar #'find-template (lookup :inherit))
           :variables (alist-plist (lookup :variables))
           :aspects   (mapcar (rcurry #'make-aspect-spec template) (lookup :aspects))
           :jobs      (mapcar (rcurry #'make-job-spec template) (lookup :jobs))))))

(defun load-template/json (pathname)
  (handler-bind ((error (lambda (condition)
                          (error "~@<Failed to load template from ~S: ~
                                  ~A~@:>"
                                 pathname condition))))
    (load-template/json-1 pathname)))

(defun load-project-spec/json-1 (pathname)
  (let+ ((spec (json:decode-json-from-source pathname))
         ((&flet lookup (name &optional (where spec))
            (cdr (assoc name where))))
         ((&flet make-version-spec (spec parent)
            (make-instance 'version-spec
                           :name      (lookup :name spec)
                           :parent    parent
                           :variables (alist-plist (lookup :variables spec)))))
         ((&flet make-job-spec (spec parent)
            (make-instance 'job-spec
                           :name       (lookup :name spec)
                           :parent     parent
                           :tags       (lookup :tags spec)
                           :conditions (lookup :conditions spec))))
         (name (lookup :name))
         (project-spec
          (let ((instance (make-instance 'project-spec)))
            (reinitialize-instance
             instance
             :name      name
             :templates (mapcar #'find-template (lookup :templates))
             :variables (alist-plist (lookup :variables))
             :versions  (mapcar (rcurry #'make-version-spec instance)
                                (lookup :versions))
             :jobs      (mapcar (rcurry #'make-job-spec instance)
                                (lookup :jobs))))))
    project-spec))

(defun load-project-spec/json (pathname)
  (handler-bind ((error (lambda (condition)
                          (error "~@<Failed to load project ~
                                  description from ~S: ~A~@:>"
                                  pathname condition))))
    (load-project-spec/json-1 pathname)))
