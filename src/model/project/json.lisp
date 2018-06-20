;;;; json.lisp --- Minimal JSON import for templates and projects.
;;;;
;;;; Copyright (C) 2012-2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.model.project)

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

(defvar *source*)

(defun source (stream)
  (flet ((make-source ()
           (apply #'text.source-location:make-source stream
                  (when-let ((file (ignore-errors (pathname stream))))
                    (list :content (read-file-into-string file))))))
    (or *source* (setf *source* (make-source)))))

(defun record-object-location (object stream start end)
  (setf (location-of object) (text.source-location:make-location
                              (source stream) start end)))

(defvar *object-members*)

(defun record-object-member-locations (object)
  (let ((bounds (nreverse (cdr (first *object-members*)))))
    (map nil (lambda+ (cell (stream start end))
               (record-object-location cell stream start end))
         object bounds))
  object)

(defvar *start*)

(defun call-with-object-result (thunk stream)
  (let* ((*start*  (file-position stream))
         (object   (funcall thunk stream))
         (end      (file-position stream))
         (location (text.source-location:make-location
                    (source stream) *start* end)))
    (setf (location-of object) location)
    object))

(defmacro with-object-result ((stream) &body body)
  (check-type stream symbol)
  `(call-with-object-result (lambda (,stream) ,@body) ,stream))

(defun %decode-json-from-source (source)
  ;; Look away, carry on. Look, this is only a temporary
  ;; solution. Right? I'm sorry.
  (handler-bind
      ((json:json-syntax-error
        (lambda (condition)
          (let+ ((control   (simple-condition-format-control condition))
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
                                        (source source) start end)
                                       "here" :kind :error)))))))
    (with-input-from-file (stream source)
      (let* ((json::*json-identifier-name-to-lisp* #'string-upcase)
             (json:*internal-decoder*              (lambda (stream)
                                                     (with-object-result (stream)
                                                       (json:decode-json stream))))

             (beginning-of-string-handler          json::*beginning-of-string-handler*)
             (json::*beginning-of-string-handler*  (lambda ()
                                                     (setf *start* (1- (file-position stream)))
                                                     (funcall beginning-of-string-handler)))

             (beginning-of-array-handler           json::*beginning-of-array-handler*)
             (json::*beginning-of-array-handler*   (lambda ()
                                                     (setf *start* (1- (file-position stream)))
                                                     (funcall beginning-of-array-handler)))

             (beginning-of-object-handler          json::*beginning-of-object-handler*)
             (json::*beginning-of-object-handler*  (lambda ()
                                                     (setf *start* (1- (file-position stream)))
                                                     (push (list *start*) *object-members*)
                                                     (funcall beginning-of-object-handler)))

             (object-key-handler                   json::*object-key-handler*)
             (json::*object-key-handler*           (lambda (object)
                                                     (let ((start (- (file-position stream)
                                                                     (length (string object))
                                                                     2)))
                                                       (push (list stream start nil)
                                                             (cdr (first *object-members*))))
                                                     (funcall object-key-handler object)))

             (object-value-handler                 json::*object-value-handler*)
             (json::*object-value-handler*         (lambda (object)
                                                     (setf (third (first (cdr (first *object-members*))))
                                                           (file-position stream))
                                                     (funcall object-value-handler object)))

             (end-of-object-handler                json::*end-of-object-handler*)
             (json::*end-of-object-handler*        (lambda ()
                                                     (setf *start* (first (first *object-members*)))
                                                     (prog1
                                                         (record-object-member-locations
                                                          (funcall end-of-object-handler))
                                                       (pop *object-members*))))

             (*source*                             nil)
             (*object-members*                     '()))
        (json:decode-json-from-source stream json:*internal-decoder*)))))

;;; Structure utilities

(deftype json-version-include-spec ()
  '(or string (cons string (cons list null))))

(defun json-list-of-version-include-specs (thing)
  (and (listp thing) (every (of-type 'json-version-include-spec) thing)))

(deftype json-project-include-spec ()
  '(cons string (satisfies json-list-of-version-include-specs)))

(defun check-name-pathname-congruence (name pathname)
  (unless (string= name (pathname-name pathname))
    (object-error
     (list (list name "name attribute" :error))
     "~@<Value of \"name\" attribute, ~S, does not match filename ~
      ~S.~@:>"
     name (pathname-name pathname)))
  name)

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
           (check-keys spec '((:minimum-generator-version nil string)
                              ,@(case name-kind
                                  (:data
                                   '((:name t string)))
                                  (:congruent
                                   `((:name nil string))))
                              ,@keys))
           (check-generator-version spec generator-version ,context)
           (let ((name ,@(ecase name-kind
                           (:data
                            `((assoc-value spec :name)))
                           (:pathname
                            `((pathname-name pathname)))
                           (:congruent
                            `((if-let ((name (assoc-value spec :name)))
                                (check-name-pathname-congruence name pathname)
                                (pathname-name pathname)))))))
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

(define-json-loader (person ((:aliases nil list) (:identities nil list)))
    (spec (name :data))
  (let* ((aliases    (lookup :aliases))
         (identities (map 'list #'puri:uri (lookup :identities)))
         (person     (apply #'rosetta-project.model.resource:make-person
                            name (append aliases identities))))
    (push person *persons*)))

;;; Template loading

(define-json-loader (one-template ((:inherit nil list) (:variables nil list)
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

(defun load-one-template/json-or-yaml (pathname &key generator-version)
  (handler-case
      (load-one-template/json
       pathname :generator-version generator-version)
    (json-syntax-error ()
      (load-one-template/yaml
       pathname :generator-version generator-version))))

(defun load-template/json-or-yaml (pathname &key generator-version)
  (let ((name (pathname-name pathname)))
    (or (find-template name :if-does-not-exist nil)
        (loading-template (name)
          (load-one-template/json-or-yaml
           pathname :generator-version generator-version)))))

;;; Project loading

(define-json-loader
    (project-spec ((:templates t list) (:variables t list) (:versions nil list) :catalog))
    (spec (name :congruent) version-test)
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

(defun load-project-spec/json-or-yaml (pathname &key generator-version version-test)
  (handler-case
      (load-project-spec/json pathname :generator-version generator-version
                                       :version-test      version-test)
    (json-syntax-error ()
      (load-project-spec/yaml pathname :generator-version generator-version
                                       :version-test      version-test))))

;;; Distribution loading

(define-json-loader (distribution ((:variables nil list) (:versions t list) :catalog))
    (spec (name :congruent))
  (let+ ((variables     (value-acons :__catalog (lookup :catalog)
                                     (process-variables (lookup :variables))))
         ;; We allow using variables defined directly in the
         ;; distribution recipe to be used in project version
         ;; expressions. Since
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
         ((&flet process-version (version note-success)
            (with-simple-restart
                (continue "~@<Continue without the project version~@:>")
              (typecase version
                (string
                 (when-let ((expansion (expand-version version note-success)))
                   (list (ensure-list expansion))))
                (cons
                 (list (list (expand-version (first version) note-success)
                             (process-variables (second version)))))))))
         ((&flet+ expand-project ((name &rest versions) note-success)
            (list* name (mapcan (rcurry #'process-version note-success)
                                versions))))
         ((&flet process-project (included-project)
            (cond
              ((not (typep included-project 'json-project-include-spec))
               (with-simple-restart
                   (continue "~@<Continue without the project entry~@:>")
                 (object-error
                  (list (list included-project "included here" :error))
                  "~@<Project entry is not a project name followed by ~
                   one or more (parametrized) project versions.~:@>")))
              ((when-let ((previous (gethash (first included-project)
                                             projects-seen)))
                 (with-simple-restart
                     (continue "~@<Ignore the additional project entry~@:>")
                   (object-error
                    (list (list previous         "initial definition"   :note)
                          (list included-project "offending definition" :error))
                    "~@<Project entry followed by another entry for ~
                     same project. Multiple project versions have to ~
                     be described in a single entry.~@:>"))))
              (t
               (let+ ((successful-expansions 0)
                      ((&flet note-success ()
                         (incf successful-expansions)))
                      ((&whole entry name &rest versions)
                       (expand-project included-project #'note-success)))
                 (with-simple-restart
                     (continue "~@<Continue without the project entry~@:>")
                   (setf (gethash name projects-seen) included-project)
                   (when (and (plusp successful-expansions) (null versions))
                     (object-error
                      (list (list included-project "specified here" :error))
                      "~@<No project versions after expansion.~@:>"))
                   (list entry))))))))
    (make-instance 'distribution-spec
                   :name      name
                   :variables variables
                   :versions  (mapcan #'process-project (lookup :versions)))))

(defun load-distribution/json-or-yaml (pathname &key generator-version)
  (handler-case
      (load-distribution/json pathname :generator-version generator-version)
    (json-syntax-error ()
      (load-distribution/yaml pathname :generator-version generator-version))))
