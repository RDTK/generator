;;;; yaml.lisp --- YAML syntax for templates and projects.
;;;;
;;;; Copyright (C) 2016-2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.model.project)

;;; Source location utilities

(defvar *locations* (make-hash-table :test #'eq))

(defvar *locations-lock* (bt:make-lock "locations"))

(defun location-of (object)
  (bt:with-lock-held (*locations-lock*)
    (gethash object *locations*)))

(defun (setf location-of) (new-value object)
  (bt:with-lock-held (*locations-lock*)
    (setf (gethash object *locations*) new-value)))

;;; Source location conditions

(define-condition annotation-condition ()
  ((annotations :initarg  :annotations
                :accessor annotations)))

(defmethod print-object :around ((object annotation-condition) stream)
  (let ((annotations (annotations object)))
    (let ((*print-circle* nil))
      (pprint-logical-block (stream annotations)
        (call-next-method)
        (loop :repeat 2 :do (pprint-newline :mandatory stream))
        (text.source-location.print::print-annotations stream annotations)))))

(define-condition simple-object-error (error
                                       annotation-condition
                                       simple-condition)
  ())

(defun object-error (annotated-objects
                     &optional format-control &rest format-arguments)
  (if-let ((annotations
            (loop :for (object text kind) :in annotated-objects
                  :for location = (location-of object)
                  :when location
                  :collect (apply #'text.source-location:make-annotation
                                  location text (when kind (list :kind kind))))))
    (error 'simple-object-error
           :annotations      annotations
           :format-control   format-control
           :format-arguments format-arguments)
    (apply #'error format-control format-arguments)))

;;; YAML syntax

(defun %load-yaml (file)
  (let* ((source  (text.source-location:make-source
                   file :content (read-file-into-string file)))
         (builder (make-instance 'text.source-location.source-tracking-builder::callback-source-tracking-builder
                                 :target   (make-instance 'language.yaml.construct::native-builder)
                                 :source   source
                                 :callback (lambda (object location)
                                             (setf (location-of object) location)))))
    (language.yaml:load file :builder builder)))

;;; Structure utilities

(defun check-keys (object &optional expected (exhaustive? t))
  (let+ ((expected (mapcar #'ensure-list expected))
         (seen     '())
         (extra    '())
         ((&flet invalid-keys (reason keys &optional cells)
            (object-error
             (if (length= 1 cells)
                 (list (list (first cells) "defined here" :error))
                 (loop :for cell :in cells
                       :for i :downfrom (length cells)
                       :collect (list cell (format nil "~:R definition" i) :error)))
             "~@<~A key~P: ~{~A~^, ~}.~@:>"
             reason (length keys) keys))))
    (map nil (lambda+ ((&whole cell key . value))
               (cond
                 ((member key seen :test #'eq :key #'car)
                  (invalid-keys "duplicate" (list key)
                                (list* cell (remove key seen
                                                    :test-not #'eq
                                                    :key      #'car))))
                 ((when-let ((info (find key expected :test #'eq :key #'first)))
                    (when-let ((type  (third info)))
                      (unless (typep value type)
                        (object-error
                         (list (list value "defined here" :error))
                         "~@<Value of the ~A attribute must be of type ~A.~@:>"
                         key type)))
                    (removef expected key :test #'eq :key #'first)
                    t))
                 (t
                  (push cell extra)))
               (push cell seen))
         object)
    (when (and exhaustive? extra)
      (invalid-keys "Unexpected" (map 'list #'first extra)
                    extra))
    (when-let ((missing (remove nil expected :key #'second)))
      (invalid-keys "Missing required" (map 'list #'first missing)
                    (list object))))
  object)

(deftype yaml-version-include-spec ()
  '(or string (cons string (cons list null))))

(defun yaml-list-of-version-include-specs (thing)
  (and (listp thing) (every (of-type 'yaml-version-include-spec) thing)))

(deftype yaml-project-include-spec ()
  '(cons string (satisfies yaml-list-of-version-include-specs)))

(defun check-generator-version (spec generator-version context)
  (when-let ((required-version (assoc-value spec :minimum-generator-version)))
    (unless (version-matches (parse-version required-version)
                             (parse-version generator-version))
      (object-error
       (list (list required-version "minimum version declaration" :info))
       "~@<The ~A requires generator version ~S, but this ~
        generator is version ~S.~@:>"
       context required-version generator-version))))

(defun process-variables (alist)
  (let ((entries (make-hash-table :test #'eq)))
    (map nil (lambda+ ((&whole cell key . &ign))
               (when (starts-with-subseq "__" (string key))
                 (object-error
                  (list (list cell "variable definition" :error))
                  "~@<Variable name ~A starts with \"__\". These ~
                   variable names are reserved for internal use.~@:>"
                  key))
               (push cell (gethash key entries)))
         alist)
    (loop :for key :being :the :hash-key :of entries
          :using (:hash-value cells)
          :unless (length= 1 cells)
          :do (object-error
               (loop :for cell :in cells
                     :for i :downfrom (length cells)
                     :collect (list cell (format nil "~:R definition" i)
                                    (if (= i 1) :note :error)))
               "~@<Multiple definitions of variable ~A.~@:>"
               key)
          :collect (value-cons key (cdr (first cells))))))

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
                   (apply #',read-name pathname args)))
             (apply #',parse-name spec name :pathname pathname args)))))))

;;; Person loading

(define-yaml-loader (person ((:aliases nil list) (:identities nil list)))
    (spec (name :data))
  (let* ((aliases    (lookup :aliases))
         (identities (map 'list #'puri:uri (lookup :identities)))
         (person     (apply #'rosetta-project.model.resource:make-person
                            name (append aliases identities))))
    (push person *persons*)))

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
        ((annotation-condition
          (lambda (condition)
            (when (eq name (first *template-load-stack*))
              (let ((annotations
                     (mappend (lambda (name)
                                (when-let ((location (location-of name)))
                                  (list (text.source-location:make-annotation
                                         location "included here" :kind :info))))
                              *template-load-stack*)))
                (appendf (annotations condition) annotations))))))
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
                               (list (make-version-spec spec instance)))))
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
