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

(defun check-generator-version (spec generator-version)
  (when-let ((required-version (cdr (assoc :minimum-generator-version spec))))
    (unless (version-matches (parse-version required-version)
                             (parse-version generator-version))
      (error "~@<The template requires generator version ~S, but ~
              this generator is version ~S.~@:>"
             required-version generator-version))))

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
    (check-generator-version spec generator-version)
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

(defun %decode-json-from-source (source)
  (let ((json::*json-identifier-name-to-lisp* #'string-upcase))
    (json:decode-json-from-source source)))
