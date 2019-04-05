;;;; yaml.lisp --- YAML syntax for templates and projects.
;;;;
;;;; Copyright (C) 2016-2019 Jan Moringen
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

(defun parse-project-include-spec (spec)
  (optima:match spec

    ;; Syntax for a single version
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

    ;; Syntax for multiple versions
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

(defun parse-distribution-include-spec (spec)
  (optima:match spec

    ((type string)
     (values spec '()))

    ((type list)
     (check-keys spec '((:name       t   string)
                        (:parameters nil list)))
     (let ((name       (assoc-value spec :name))
           (parameters (assoc-value spec :parameters)))
       (values name (process-variables parameters))))

    (otherwise
     (object-error
      (list (list spec "specified here" :error))
      "~@<Include specification is neither a distribution name nor a ~
       dictionary with keys \"name\" and optionally ~
       \"parameters\"."))))

;;; Includes

(defun call-with-loading-recipe (thunk stack-symbol name)
  (symbol-macrolet ((stack (symbol-value stack-symbol)))
    (flet ((error-objects (&optional (stack stack))
             (map 'list (lambda (name)
                          (list name "included here" :info))
                  stack)))
      (when (member name stack :test #'string=)
        (let ((augmented-stack (list* name stack)))
          (object-error (error-objects augmented-stack)
                        "~@<Cyclic includes~
                         ~@:_~@:_~
                         ~4@T~{~
                           ~A~^~@:_~@T->~@T~
                         ~}~@:>"
                        (reverse augmented-stack))))
      (progv (list stack-symbol) (list (list* name stack))
        (handler-bind
            (((and error (not annotation-condition))
               (lambda (condition)
                 (object-error (error-objects) "~A" condition))))
          (funcall thunk))))))

(defmacro loading-recipe ((stack-var name) &body body)
  `(call-with-loading-recipe (lambda () ,@body) ',stack-var ,name))

;;; Loader definition macro

(defmacro define-yaml-loader ((concept keys) (spec-var name &rest args)
                              &body body)
  (check-type spec-var symbol)
  (check-type name (cons symbol (cons (member :data :pathname) null)))
  (let+ (((&optional name-var name-kind) name)
         (other-args (set-difference
                      args '(repository pathname generator-version)
                      :test #'eq))
         (all-args   (list* 'repository 'pathname 'generator-version
                            other-args))
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
                           (:data     `((assoc-value spec :name)))
                           (:pathname `((pathname-name pathname))))))
             (values spec name pathname))))

       (defun ,parse-name (,spec-var ,name-var &key ,@all-args)
         (declare (ignore ,@(set-difference all-args args)))
         (let+ (((&flet lookup (name &optional (where ,spec-var))
                   (cdr (assoc name where)))))
           ,@body))

       (defun ,load-name (pathname
                          &rest args
                          &key (repository (missing-required-argument :repository))
                               generator-version
                               ,@other-args)
         (declare (ignore generator-version ,@other-args))
         (handler-bind (((and error (not annotation-condition))
                         (lambda (condition)
                           (error "~@<Error when loading ~(~A~) ~
                                   description from ~S: ~A~@:>"
                                  ',concept
                                  (jenkins.util:safe-enough-namestring pathname)
                                  condition))))
           (let+ (((&values spec name pathname)
                   (apply #',read-name pathname
                          (remove-from-plist args :repository))))
             (copy-location
              spec (apply #',parse-name spec name
                          :repository repository
                          :pathname   pathname
                          args))))))))

;;; Person loading

(define-yaml-loader (person ((:aliases    nil list)
                             (:identities nil list)
                             (:variables  nil list)))
    (spec (name :data))
  (let* ((aliases    (lookup :aliases))
         (identities (map 'list #'puri:uri (lookup :identities)))
         (variables  (process-variables (lookup :variables)))
         (person     (change-class
                      (apply #'rosetta-project.model.resource:make-person
                             name (append aliases identities))
                      'person :variables           variables
                              :explicit-names      (list* name aliases)
                              :explicit-identities identities)))
    (push person *persons*)
    person))

;;; Template loading

(define-yaml-loader (one-template ((:inherit nil list) (:variables nil list)
                                   (:aspects nil list) (:jobs nil list)))
    (spec (name :pathname) repository generator-version)
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
         ;; Inherit
         (inherit-seen (make-uniqueness-checker
                        "~@<Duplicate inherit specification.~@:>"))
         ((&flet process-inherit (name)
            (with-uniqueness-check (inherit-seen name name)
              (resolve-template-dependency
               name repository :generator-version generator-version))))
         (template (make-instance 'template)))
    ;; Load required templates and finalize the object.
    (setf (find-template name)
          (reinitialize-instance
           template
           :name      name
           :inherit   (mapcar #'process-inherit (lookup :inherit))
           :variables (process-variables (lookup :variables))
           :aspects   (mapcar (rcurry #'make-aspect-spec template) (lookup :aspects))
           :jobs      (mapcar (rcurry #'make-job-spec template) (lookup :jobs))))))

(defvar *template-load-stack* '())

(defun find-or-load-template (name pathname repository generator-version)
  (or (find-template name :if-does-not-exist nil)
      (loading-recipe (*template-load-stack* name)
        (load-one-template/yaml
         pathname
         :repository        repository
         :generator-version generator-version))))

(defun resolve-template-dependency (name repository &key generator-version)
  (let ((pathname (recipe-path repository :template name)))
    (find-or-load-template name pathname repository generator-version)))

(defun load-template/yaml (pathname &key repository generator-version)
  (let ((name (pathname-name pathname)))
    (find-or-load-template name pathname repository generator-version)))

;;; Project loading

(defun compile-pattern (pattern)
  (handler-case
      (ppcre:create-scanner pattern)
    (error (condition)
      (object-error (list (list pattern "defined here" :error))
                    "~@<Invalid version pattern: ~A~@:>" condition))))

(let+ (((&flet make-match-variable (index)
          (format-symbol :keyword "MATCH:~D" index)))
       (match-variables (map 'vector #'make-match-variable (iota 10))))
  (defun match-variable (index)
    (if (< index (length match-variables))
        (aref match-variables index)
        (make-match-variable index))))

(defun apply-version-pattern (requested-name pattern)
  (let+ (((&values match groups) (ppcre:scan-to-strings
                                  pattern requested-name)))
    (list* (cons :|MATCH:0| match)
           (loop :for group :across groups
                 :for i :from 1
                 :collect (cons (match-variable i) group)))))

(assert (equal (apply-version-pattern "0.15-famula" "^([0-9]+\.[0-9]+)-(.*)$")
               '((:|MATCH:0| . "0.15-famula")
                 (:|MATCH:1| . "0.15")
                 (:|MATCH:2| . "famula"))))

(define-yaml-loader
    (project-spec ((:templates t list) (:variables t list) (:versions nil list) :catalog))
    (spec (name :pathname) version-test)
  (let+ (((&flet make-version-spec (spec parent requested-names)
            (check-keys spec '((:name      t   string :conflicts :pattern)
                               (:pattern   t   string :conflicts :name)
                               (:variables nil list)
                               :catalog))
            (let* ((name      (lookup :name spec))
                   (pattern   (lookup :pattern spec))
                   (variables (process-variables (lookup :variables spec)))
                   (catalog   (lookup :catalog spec))
                   (variables (if catalog
                                  (var:value-acons :__catalog catalog
                                                   variables)
                                  variables)))
              (if pattern
                  (map 'list (lambda (requested-name)
                               (make-instance 'version-spec
                                              :name      requested-name
                                              :parent    parent
                                              :variables (nconc (apply-version-pattern
                                                                 requested-name pattern)
                                                                variables)))
                       requested-names)
                  (list (make-instance 'version-spec
                                       :name      name
                                       :parent    parent
                                       :variables variables))))))
         ;; Templates
         (templates-seen (make-uniqueness-checker "~@<Duplicate template.~@:>"))
         ((&flet process-template (name)
            (with-uniqueness-check (templates-seen name name)
              (handler-bind
                  ((error (lambda (error)
                            (object-error
                             (list (list name "included here" :info))
                             "~A" error))))
                (find-template name)))))
         (instance (make-instance 'project-spec :name name)))
    (reinitialize-instance
     instance
     :templates (map 'list #'process-template (lookup :templates))
     :variables (var:value-acons
                 :__catalog (lookup :catalog)
                 (process-variables (lookup :variables)))
     :versions  (mappend (lambda (spec)
                           (with-simple-restart
                               (continue "~@<Ignore version entry.~@:>")
                             (check-keys
                              spec
                              '((:name    t string :conflicts :pattern)
                                (:pattern t string :conflicts :name))
                              nil)
                             (let ((name    (lookup :name spec))
                                   (pattern (when-let ((pattern (lookup :pattern spec)))
                                              (compile-pattern pattern))))
                               (when-let ((requested-versions
                                           (or (not version-test)
                                               (funcall version-test name pattern))))
                                 (let ((version-specs (make-version-spec
                                                       spec instance requested-versions)))
                                   (map nil (curry #'copy-location spec)
                                        version-specs)
                                   version-specs)))))
                         (lookup :versions)))))

;;; Distribution loading

(define-yaml-loader (one-distribution ((:include   nil list)
                                       (:variables nil list)
                                       (:versions  t   list)
                                       :catalog))
    (spec (name :pathname) repository generator-version)
  (let+ ((variables (var:value-acons :__catalog (lookup :catalog)
                                     (process-variables (lookup :variables))))
         ;; We allow using variables defined directly in the
         ;; distribution recipe to be used in project version
         ;; expressions.
         (context   (make-instance 'var:direct-variables-mixin
                                   :variables variables))
         ((&flet expand-expression (expression &optional note-success)
            (handler-case
                (prog1
                    (var:evaluate context (var:value-parse expression))
                  (when note-success (funcall note-success)))
              (error (condition)
                (object-error
                 (list (list expression "specified here" :error))
                 "~@<Failed to evaluate include expression: ~A~@:>"
                 condition)))))
         ;; Distribution includes
         (includes-seen (make-uniqueness-checker
                         "~@<Duplicate distribution include.~@:>"))
         ((&flet process-include (spec)
            (with-simple-restart
                (continue "~@<Continue without including the ~
                           distribution~@:>")
              (let+ (((&values name parameters)
                      (parse-distribution-include-spec spec))
                     (name         (expand-expression name))
                     (distribution (with-uniqueness-check
                                       (includes-seen name spec)
                                     (resolve-distribution-dependency
                                      name repository
                                      :generator-version generator-version))))
                (list (make-instance 'distribution-include
                                     :distribution distribution
                                     :variables    parameters))))))
         ;; Project includes
         (projects-seen (make-uniqueness-checker
                         "~@<Project entry followed by another entry ~
                          for same project. Multiple project versions ~
                          have to be described in a single ~
                          entry.~@:>"))
         ((&flet+ process-version ((name . parameters) note-success)
            (with-simple-restart
                (continue "~@<Continue without the project version~@:>")
              (list (list (expand-expression name note-success)
                          (when parameters (process-variables parameters)))))))
         ((&flet expand-project (name versions note-success)
            (list* name (mapcan (rcurry #'process-version note-success)
                                versions))))
         ((&flet process-project (included-project)
            (with-simple-restart
                (continue "~@<Continue without the project entry~@:>")
              (let+ (((&values name versions)
                      (parse-project-include-spec included-project)))
                (funcall projects-seen name)
                (let+ ((successful-expansions 0)
                       ((&flet note-success ()
                          (incf successful-expansions)))
                       ((name &rest versions)
                        (expand-project name versions #'note-success)))
                  (funcall projects-seen name included-project)
                  (when (and (plusp successful-expansions) (null versions))
                    (object-error
                     (list (list included-project "specified here" :error))
                     "~@<No project versions after expansion.~@:>"))
                  (map 'list (lambda+ ((version parameters))
                               (make-instance 'project-include
                                              :project   name
                                              :version   version
                                              :variables parameters))
                       versions)))))))
    (make-instance
     'distribution-spec
     :name            name
     :direct-includes (mapcan #'process-include (lookup :include))
     :variables       variables
     :direct-versions (mapcan #'process-project (lookup :versions)))))

(defvar *distributions* (make-hash-table :test #'equal))

(defun find-distribution (name)
  (gethash name *distributions*))

(defun ensure-distribution (name thunk)
  (ensure-gethash name *distributions* (funcall thunk)))

(defvar *distribution-load-stack* '())

(defun find-or-load-distribution (name pathname repository generator-version)
  (ensure-distribution
   name (lambda ()
          (loading-recipe (*distribution-load-stack* name)
            (load-one-distribution/yaml
             pathname
             :repository        repository
             :generator-version generator-version)))))

(defun resolve-distribution-dependency (name repository &key generator-version)
  (let ((pathname (recipe-path repository :distribution name)))
    (find-or-load-distribution name pathname repository generator-version)))

(defun load-distribution/yaml (pathname
                               &key
                               (repository (missing-required-argument :repository))
                               generator-version)
  (let ((name (pathname-name pathname)))
    (find-or-load-distribution name pathname repository generator-version)))
