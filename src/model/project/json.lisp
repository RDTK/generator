;;;; json.lisp --- Minimal JSON import for templates and projects.
;;;;
;;;; Copyright (C) 2012-2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.model.project)

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

;;; JSON syntax

(define-condition json-syntax-error (error
                                     annotation-condition
                                     more-conditions:chainable-condition)
  ()
  (:report
   (lambda (condition stream)
     (let ((cause (more-conditions:cause condition)))
       (apply #'format stream (simple-condition-format-control cause)
              (simple-condition-format-arguments cause))))))

(defun %decode-json-from-source (source)
  ;; Look away, carry on. Look, this is only a temporary
  ;; solution. Right? I'm sorry.
  (handler-bind
      ((json:json-syntax-error
        (lambda (condition)
          (let+ ((source    (text.source-location:make-source
                             source :content (read-file-into-string source)))
                 (control   (simple-condition-format-control condition))
                 (arguments (simple-condition-format-arguments condition))
                 (position  (json::stream-error-stream-file-position condition))
                 ((&values start end)
                  (cond
                    ((and (stringp control)
                          (eql 0 (search "Invalid JSON literal" control))
                          (typep arguments '(cons string)))
                     (values (- position (length (first arguments))) position))
                    ((and (stringp control)
                          (eql 0 (search "Invalid char on JSON input" control)))
                     (values position (1+ position)))
                    (t
                     (values (max 0 (1- position)) (max 1 position))))))
            (error 'json-syntax-error
                   :cause       condition
                   :annotations (list (text.source-location:make-annotation
                                       (text.source-location:make-location
                                        source start end)
                                       "here" :kind :error)))))))
    (let ((json::*json-identifier-name-to-lisp* #'string-upcase))
      (json:decode-json-from-source source))))

;;; Structure utilities

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

;;; Loader definition macro

(defmacro define-json-loader ((concept keys) (spec-var name &rest args)
                              &body body)
  (check-type spec-var symbol)
  (check-type name (cons symbol (cons (member :data :pathname :congruent) null)))
  (let+ (((&optional name-var name-kind) name)
         (other-args (set-difference args '(pathname generator-version)
                                     :test #'eq))
         (all-args   (list* 'pathname 'generator-version other-args))
         (read-name  (symbolicate '#:read-  concept '#:/json))
         (parse-name (symbolicate '#:parse- concept '#:/json))
         (load-name  (symbolicate '#:load-  concept '#:/json))
         (context    (format nil "~(~A~) recipe" concept)))
    `(progn
       (defun ,read-name (pathname &key generator-version ,@other-args)
         (declare (ignore ,@other-args))
         (let ((spec (%decode-json-from-source pathname)))
           (check-generator-version spec generator-version ,context)
           (check-keys spec '(:minimum-generator-version
                              ,@(when (member name-kind '(:data :congruent))
                                  '((:name . t)))
                              ,@keys))
           (let ((name ,@(ecase name-kind
                           (:data
                            `((assoc-value spec :name)))
                           (:pathname
                            `((pathname-name pathname)))
                           (:congruent
                            `((let ((name (assoc-value spec :name)))
                                (check-name-pathname-congruence name pathname)))))))
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

(define-json-loader (person (:aliases :identities))
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
      (loading-template (name)
        (load-one-template/json (make-pathname :name name :defaults context)
                                :generator-version generator-version))))

(define-json-loader (one-template (:inherit :variables :aspects :jobs))
    (spec (name :pathname) pathname generator-version)
  (let+ (((&flet make-aspect-spec (spec parent)
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
  (let ((name (pathname-name pathname)))
    (or (find-template name :if-does-not-exist nil)
        (loading-template (name)
          (load-one-template/json
           pathname :generator-version generator-version)))))

;;; Project loading

(define-json-loader
    (project-spec ((:templates . t) (:variables . t) :versions :catalog))
    (spec (name :congruent) version-test)
  (let+ (((&flet make-version-spec (spec parent)
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
         (instance (make-instance 'project-spec :name name)))
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
                            (lookup :versions))))))

;;; Distribution loading

(define-json-loader (distribution (:variables (:versions . t) :catalog))
    (spec (name :congruent))
  (let+ ((projects-seen (make-hash-table :test #'equal))
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
    (make-instance 'distribution-spec
                   :name      name
                   :variables (value-acons :__catalog (lookup :catalog)
                                           (process-variables (lookup :variables)))
                   :versions  (mapcan #'process-project (lookup :versions)))))
