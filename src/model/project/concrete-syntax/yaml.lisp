;;;; yaml.lisp --- YAML syntax for templates and projects.
;;;;
;;;; Copyright (C) 2016-2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.model.project)

(defun %load-yaml (file)
  (let* ((source  (text.source-location:make-source
                   file :content (read-file-into-string file)))
         (builder (make-builder source)))
    (handler-case
        (language.yaml:load file :builder builder)
      (esrap:esrap-parse-error (condition)
        (let ((start (esrap:esrap-error-position condition)))
         (error 'yaml-syntax-error
                :cause       condition
                :annotations (list (text.source-location:make-annotation
                                    (text.source-location:make-location
                                     source start (1+ start))
                                    "here" :kind :error))))))))

;;; Structure utilities

(deftype yaml-version-include-spec ()
  '(or string (cons string (cons list null))))

(defun yaml-list-of-version-include-specs (thing)
  (and (listp thing) (every (of-type 'yaml-version-include-spec) thing)))

(deftype yaml-project-include-spec ()
  '(cons string (satisfies yaml-list-of-version-include-specs)))

(defun parse-include-spec (spec)
  (optima:match spec

    ;; Legacy syntax variants
    ((list* (and name (type string)) versions)
     (flet ((parse-version (spec)
              (optima:match spec
                ((type string)
                 (cons spec '()))
                ((list (and name (type string))
                       (and parameters (type list)))
                 (cons name parameters))
                (otherwise
                 (object-error
                  (list (list spec "specified here" :error))
                  "~@<Version is neither a string nor a list of a ~
                   string followed by a dictionary of ~
                   parameters.~@:>")))))
       (values name (map 'list #'parse-version versions))))

    ;; Current syntax for a single version
    ((optima:guard (type string) (position #\@ spec)) ; TODO check length
     (let* ((index   (position #\@ spec))
            (name    (string-right-trim '(#\Space) (subseq spec 0 index)))
            (version (string-right-trim '(#\Space) (subseq spec (1+ index)))))
       (setf (location-of name)    (location-of spec)
             (location-of version) (location-of spec))
       (values name (list (cons version nil)))))

    ((and (assoc :name    (and name    (type string)))
          (assoc :version (and version (type string)))
          (or (assoc :parameters (and parameters (type list)))
              (and)))
     (check-keys spec '((:name       t   string)
                        (:version    t   string)
                        (:parameters nil list)))
     (values name (list (cons version parameters))))

    ;; Current syntax for multiple versions
    ((and (assoc :name     (and name     (type string)))
          (assoc :versions (and versions (type list))))
     (check-keys spec '((:name t string) (:versions t list)))
     (flet ((parse-version (spec)
              (optima:match spec
                ((and (assoc :version (and version (type string)))
                      (or (assoc :parameters (and parameters (type list)))
                          (and)))
                 (check-keys spec '((:version    t   string)
                                    (:parameters nil list)))
                 (cons version parameters))
                (otherwise
                 (object-error
                  (list (list spec "specified here" :error))
                  "~@<Project version entry is not a dictionary with ~
                   keys \"version\" and optionally ~
                   \"parameters\".~:@>")))))
       (values name (map 'list #'parse-version versions))))

    (otherwise
     (object-error
      (list (list spec "specified here" :error))
      "~@<Project entry is neither a list consisting of a project name ~
       followed by one or more (parametrized) project versions nor a ~
       string of the form NAME@VERSION nor a dictionary with keys ~
       \"name\" and \"version\" or \"versions\".~:@>"))))

;;; Loader definition macro

(defmacro define-yaml-loader ((concept keys) (spec-var name &rest args)
                              &body body)
  (check-type spec-var symbol)
  (check-type name (cons symbol (cons (member :data :pathname) null)))
  (let+ (((&optional name-var name-kind) name)
         (other-args (set-difference args '(pathname generator-version)
                                     :test #'eq))
         (all-args   (list* 'pathname 'generator-version other-args))
         (read-name  (symbolicate '#:read-  concept '#:/yaml))
         (parse-name (symbolicate '#:parse- concept '#:/yaml))
         (load-name  (symbolicate '#:load-  concept '#:/yaml))
         (context    (format nil "~(~A~) recipe" concept)))
    `(progn
       (defun ,read-name (pathname &key generator-version ,@other-args)
         (declare (ignore ,@other-args))
         (let ((spec (%load-yaml pathname)))
           (check-keys spec '((:minimum-generator-version nil string)
                              ,@(when (eq name-kind :data)
                                  '((:name t string)))
                              ,@keys))
           (check-generator-version spec generator-version ,context)
           (let ((name ,@(ecase name-kind
                           (:data
                            `((assoc-value spec :name)))
                           (:pathname
                            `((pathname-name pathname))))))
             (values spec name pathname))))

       (defun ,parse-name (,spec-var ,name-var &key ,@all-args)
         (declare (ignore ,@(set-difference all-args args)))
         (let+ (((&flet lookup (name &optional (where ,spec-var))
                   (cdr (assoc name where)))))
           ,@body))

       (defun ,load-name (pathname &rest args &key generator-version ,@other-args)
         (declare (ignore generator-version ,@other-args))
         (handler-bind (((and error (not annotation-condition))
                         (lambda (condition)
                           (error "~@<Error when loading ~(~A~) ~
                                   description from ~S: ~A~@:>"
                                  ',concept pathname condition))))
           (let+ (((&values spec name pathname)
                   (apply #',read-name pathname args))
                  (result (apply #',parse-name spec name :pathname pathname
                                 args)))
             (setf (location-of result) (location-of spec))
             result))))))

;;; Person loading

(define-yaml-loader (person ((:aliases nil list) (:identities nil list)))
    (spec (name :data))
  (let* ((aliases    (lookup :aliases))
         (identities (map 'list #'puri:uri (lookup :identities)))
         (person     (apply #'rosetta-project.model.resource:make-person
                            name (append aliases identities))))
    (push person *persons*)
    person))

;;; Template loading

(defvar *template-load-stack* '())

(defun call-with-loading-template (thunk name)
  (when (member name *template-load-stack* :test #'string=)
    (object-error
     (map 'list (lambda (name)
                  (list name "included here" :info))
          (list* name *template-load-stack*))
     "~@<Cyclic template inheritance~
      ~@:_~@:_~
      ~4@T~{~
        ~A~^~@:_~@T->~@T~
      ~}~@:>"
     (reverse (list* name *template-load-stack*))))
  (let ((*template-load-stack* (list* name *template-load-stack*)))
    (handler-bind
        (((and error (not annotation-condition))
          (lambda (condition)
            (let* ((condition (make-condition 'simple-object-error
                                              :format-control   "~A"
                                              :format-arguments (list condition)))
                   (annotations
                     (mappend (lambda (name)
                                (when-let ((location (location-of name)))
                                  (list (text.source-location:make-annotation
                                         location "included here" :kind :info))))
                              *template-load-stack*)))
              (appendf (annotations condition) annotations)
              (error condition)))))
      (funcall thunk))))

(defmacro loading-template ((name) &body body)
  `(call-with-loading-template (lambda () ,@body) ,name))

(defun resolve-template-dependency (name context &key generator-version)
  (or (find-template name :if-does-not-exist nil)
      (loading-template (name)
        (load-one-template/json-or-yaml (make-pathname :name name :defaults context)
                                        :generator-version generator-version))))

(define-yaml-loader (one-template ((:inherit nil list) (:variables nil list)
                                   (:aspects nil list) (:jobs nil list)))
    (spec (name :pathname) pathname generator-version)
  (let+ (((&flet make-aspect-spec (spec parent)
            (check-keys spec '((:name       t   string)
                               (:aspect     t   string)
                               (:variables  nil list)
                               (:conditions nil list)))
            (make-instance 'aspect-spec
                           :name       (lookup :name spec)
                           :parent     parent
                           :aspect     (lookup :aspect spec)
                           :variables  (process-variables (lookup :variables spec))
                           :conditions (lookup :conditions spec))))
         ((&flet make-job-spec (spec parent)
            (check-keys spec '((:name       t   string)
                               (:variables  nil list)
                               (:conditions nil list)))
            (make-instance 'job-spec
                           :name       (lookup :name spec)
                           :parent     parent
                           :variables  (process-variables (lookup :variables spec))
                           :conditions (lookup :conditions spec))))
         (template (make-instance 'template)))
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

(defun load-template/yaml (pathname &key generator-version)
  (let ((name (pathname-name pathname)))
    (or (find-template name :if-does-not-exist nil)
        (loading-template (name)
          (load-one-template/yaml
           pathname :generator-version generator-version)))))

;;; Project loading

(define-yaml-loader
    (project-spec ((:templates t list) (:variables t list) (:versions nil list) :catalog))
    (spec (name :pathname) version-test)
  (let+ (((&flet make-version-spec (spec parent)
            (check-keys spec '((:name      t   string)
                               (:variables nil list)
                               :catalog))
            (let ((catalog   (lookup :catalog spec))
                  (variables (process-variables (lookup :variables spec))))
              (make-instance 'version-spec
                             :name      (lookup :name spec)
                             :parent    parent
                             :variables (if catalog
                                            (value-acons :__catalog catalog
                                                         variables)
                                            variables)))))
         (instance (make-instance 'project-spec :name name)))
    (reinitialize-instance
     instance
     :templates (mapcar (lambda (name)
                          (handler-bind
                              ((error (lambda (error)
                                        (object-error
                                         (list (list name "included here" :info))
                                         "~A" error))))
                            (find-template name)))
                        (lookup :templates))
     :variables (value-acons
                 :__catalog (lookup :catalog)
                 (process-variables (lookup :variables)))
     :versions  (mappend (lambda (spec)
                           (with-simple-restart
                               (continue "~@<Ignore version entry.~@:>")
                             (check-keys spec '((:name t string)) nil)
                             (when (or (not version-test)
                                       (let ((name (lookup :name spec)))
                                         (funcall version-test name)))
                               (let ((version-spec (make-version-spec
                                                    spec instance)))
                                 (setf (location-of version-spec)
                                       (location-of spec))
                                 (list version-spec)))))
                         (lookup :versions)))))

;;; Distribution loading

(define-yaml-loader (distribution ((:variables nil list) (:versions t list) :catalog))
    (spec (name :pathname))
  (let+ ((variables     (value-acons :__catalog (lookup :catalog)
                                     (process-variables (lookup :variables))))
         ;; We allow using variables defined directly in the
         ;; distribution recipe to be used in project version
         ;; expressions.
         (context       (make-instance 'direct-variables-mixin
                                       :variables variables))
         (projects-seen (make-hash-table :test #'equal))
         ((&flet expand-version (expression note-success)
            (handler-case
                (prog1
                    (evaluate context (value-parse expression))
                  (funcall note-success))
              (error (condition)
                (object-error
                 (list (list expression "specified here" :error))
                 "~@<Failed to evaluate version of included project: ~A~@:>"
                 condition)))))
         ((&flet+ process-version ((name . parameters) note-success)
            (with-simple-restart
                (continue "~@<Continue without the project version~@:>")
              (list (list (expand-version name note-success)
                          (when parameters (process-variables parameters)))))))
         ((&flet expand-project (name versions note-success)
            (list* name (mapcan (rcurry #'process-version note-success)
                                versions))))
         ((&flet process-project (included-project)
            (with-simple-restart
                (continue "~@<Continue without the project entry~@:>")
              (let+ (((&values name versions)
                      (parse-include-spec included-project)))
                (when-let ((previous (gethash name projects-seen)))
                  (object-error
                   (list (list previous         "initial definition"   :note)
                         (list included-project "offending definition" :error))
                   "~@<Project entry followed by another entry for ~
                     same project. Multiple project versions have to ~
                     be described in a single entry.~@:>"))
                (let+ ((successful-expansions 0)
                       ((&flet note-success ()
                          (incf successful-expansions)))
                       ((&whole entry name &rest versions)
                        (expand-project name versions #'note-success)))
                  (setf (gethash name projects-seen) included-project)
                  (when (and (plusp successful-expansions) (null versions))
                    (object-error
                     (list (list included-project "specified here" :error))
                     "~@<No project versions after expansion.~@:>"))
                  (list entry)))))))
    (make-instance 'distribution-spec
                   :name      name
                   :variables variables
                   :versions  (mapcan #'process-project (lookup :versions)))))
