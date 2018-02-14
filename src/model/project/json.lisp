;;;; json.lisp --- Minimal JSON import for templates and projects.
;;;;
;;;; Copyright (C) 2012-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.model.project)

(deftype json-version-include-spec ()
  '(or string (cons string (cons list null))))

(defun json-list-of-version-include-specs (thing)
  (and (listp thing) (every (of-type 'json-version-include-spec) thing)))

(deftype json-project-include-spec ()
  '(cons string (satisfies json-list-of-version-include-specs)))

(defun check-generator-version (spec generator-version context)
  (when-let ((required-version (assoc-value spec :minimum-generator-version)))
    (unless (version-matches (parse-version required-version)
                             (parse-version generator-version))
      (error "~@<The ~A requires generator version ~S, but ~
              this generator is version ~S.~@:>"
             context required-version generator-version))))

(defun check-name-pathname-congruence (name pathname)
  (unless (string= name (pathname-name pathname))
    (error "~@<Value of \"name\" attribute, ~S, does not match ~
           filename ~S.~@:>"
           name (pathname-name pathname)))
  name)

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
                        ~{~A~^, ~}.~@:>"
                       key (mapcar #'json:encode-json-to-string value)))
       :collect (value-cons key (first value)))))

;;; Person loading

(defun load-person/json (pathname &key generator-version)
  (let+ ((spec (%decode-json-from-source pathname))
         ((&flet lookup (name &optional (where spec))
            (cdr (assoc name where)))))
    (check-generator-version spec generator-version "person recipe")
    (check-keys spec '(:minimum-generator-version
                       (:name t) :aliases :identities)
                t)
    (let* ((name       (lookup :name))
           (aliases    (lookup :aliases))
           (identities (map 'list #'puri:uri (lookup :identities)))
           (person     (apply #'rosetta-project.model.resource:make-person
                              name (append aliases identities))))
      (push person *persons*))))

;;; Template loading

(defvar *template-load-stack* '())

(defun call-with-loading-template (thunk name)
  (when (member name *template-load-stack* :test #'string=)
    (error "~@<Cyclic template inheritance~
            ~@:_~@:_~
            ~4@T~{~
              ~A~^~@:_~@T->~@T~
            ~}~@:>"
           (reverse (list* name *template-load-stack*))))
  (let ((*template-load-stack* (list* name *template-load-stack*)))
    (funcall thunk)))

(defmacro loading-template ((name) &body body)
  `(call-with-loading-template (lambda () ,@body) ,name))

(defun resolve-template-dependency (name context &key generator-version)
  (or (find-template name :if-does-not-exist nil)
      (load-template/json (make-pathname :name name :defaults context)
                          :generator-version generator-version)))

(defun load-template/json-1 (pathname &key generator-version)
  (let+ ((name (pathname-name pathname))
         (spec (%decode-json-from-source pathname))
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


         (template (make-instance 'template)))
    (check-generator-version spec generator-version "template")
    (check-keys spec '(:minimum-generator-version
                       :inherit :variables :aspects :jobs))
    ;; Load required templates and finalize the object.
    (setf (find-template name)
          (reinitialize-instance
           template
           :name      name
           :inherit   (mapcar (rcurry #'resolve-template-dependency
                                      pathname :generator-version generator-version)
                              (lookup :inherit))
           :variables (process-variables (lookup :variables))
           :aspects   (mapcar (rcurry #'make-aspect-spec template) (lookup :aspects))
           :jobs      (mapcar (rcurry #'make-job-spec template) (lookup :jobs))))))

(defun load-template/json (pathname &key generator-version)
  (handler-bind ((error (lambda (condition)
                          (error "~@<Error when loading template from ~
                                  ~S: ~A~@:>"
                                 pathname condition))))
    (let ((name (pathname-name pathname)))
      (or (find-template name :if-does-not-exist nil)
          (loading-template (name)
            (load-template/json-1 pathname :generator-version generator-version))))))

(defun load-project-spec/json-1 (pathname &key version-test generator-version)
  (let+ ((spec (%decode-json-from-source pathname))
         ((&flet lookup (name &optional (where spec))
            (cdr (assoc name where))))
         ((&flet make-version-spec (spec parent)
            (check-keys spec '((:name . t) :variables :catalog))
            (let ((catalog   (lookup :catalog spec))
                  (variables (process-variables (lookup :variables spec))))
              (make-instance 'version-spec
                             :name      (lookup :name spec)
                             :parent    parent
                             :variables (if catalog
                                            (value-acons :__catalog catalog
                                                         variables)
                                            variables)))))
         (name (lookup :name)))
    (check-generator-version spec generator-version "project recipe")
    (check-keys spec '(:minimum-generator-version
                       (:name . t) (:templates . t) (:variables . t)
                       :versions :catalog))
    (check-name-pathname-congruence name pathname)
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

(defun load-project-spec/json (pathname &key version-test generator-version)
  (handler-bind ((error (lambda (condition)
                          (error "~@<Error when loading project ~
                                  description from ~S: ~A~@:>"
                                  pathname condition))))
    (load-project-spec/json-1 pathname
                              :version-test      version-test
                              :generator-version generator-version)))

(defun load-distribution-spec/json-1 (pathname &key generator-version)
  (let+ ((spec (%decode-json-from-source pathname))
         ((&flet lookup (name &optional (where spec))
            (cdr (assoc name where))))
         (name (lookup :name))
         (projects-seen (make-hash-table :test #'equal))
         ((&flet process-version (version)
            (typecase version
              (string
               (list version))
              (cons
               (list (first version) (process-variables (second version)))))))
         ((&flet process-project (included-project)
            (cond
              ((not (typep included-project 'json-project-include-spec))
               (cerror "~@<Continue without the project entry~@:>"
                       "~@<Project entry~
                        ~@:_~@:_~
                        ~2@T~A~
                        ~@:_~@:_~
                        is not a project name followed by one or more ~
                        project (parametrized) versions.~:@>"
                       (json:encode-json-to-string included-project)))
              ((when-let ((previous (gethash (first included-project)
                                             projects-seen)))
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
                         (json:encode-json-to-string included-project))))
              (t
               (let+ (((name &rest versions) included-project))
                 (setf (gethash name projects-seen) included-project)
                 (list (list* name (map 'list #'process-version versions)))))))))
    (check-generator-version spec generator-version "distribution recipe")
    (check-keys spec '(:minimum-generator-version
                       (:name . t) :variables (:versions . t)
                       :catalog))
    (check-name-pathname-congruence name pathname)
    (make-instance 'distribution-spec
                   :name      name
                   :variables (value-acons :__catalog (lookup :catalog)
                                           (process-variables (lookup :variables)))
                   :versions  (mapcan #'process-project (lookup :versions)))))

(defun load-distribution/json (pathname &key generator-version)
  (handler-bind ((error (lambda (condition)
                          (error "~@<Error when loading distribution ~
                                  description from ~S: ~A~@:>"
                                 pathname condition))))
    (load-distribution-spec/json-1 pathname
                                   :generator-version generator-version)))

(defun %decode-json-from-source (source)
  (let ((json::*json-identifier-name-to-lisp* #'string-upcase))
    (json:decode-json-from-source source)))
