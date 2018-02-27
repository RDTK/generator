;;;; json.lisp --- Minimal JSON import for templates and projects.
;;;;
;;;; Copyright (C) 2012-2018 Jan Moringen
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

(defvar *cell-start*)

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
                                                     (push (list nil) *object-members*)
                                                     (funcall beginning-of-object-handler)))

             (object-key-handler                   json::*object-key-handler*)
             (json::*object-key-handler*           (lambda (object)
                                                     (setf *cell-start* *start*)
                                                     (funcall object-key-handler object)))

             (object-value-handler                 json::*object-value-handler*)
             (json::*object-value-handler*         (lambda (object)
                                                     (push (list stream *cell-start* (file-position stream))
                                                           (cdr (first *object-members*)))
                                                     (funcall object-value-handler object)))

             (end-of-object-handler                json::*end-of-object-handler*)
             (json::*end-of-object-handler*        (lambda ()
                                                     (prog1
                                                         (record-object-member-locations
                                                          (funcall end-of-object-handler))
                                                       (pop *object-members*))))

             (*source*                             nil)
             (*object-members*                     '()))
        (json:decode-json-from-source stream json:*internal-decoder*)))))

;;; Structure utilities

(defun check-keys (object &optional expected (exhaustive? t))
  (let+ ((seen     '())
         (expected (mapcar #'ensure-list expected))
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
    (map nil (lambda+ ((&whole cell key . &ign))
               (cond
                 ((member key seen :test #'eq :key #'car)
                  (invalid-keys "duplicate" (list key)
                                (list* cell (remove key seen
                                                    :test-not #'eq
                                                    :key      #'car))))
                 ((member key expected :test #'eq :key #'car)
                  (removef expected key :test #'eq :key #'car))
                 (t
                  (push cell extra)))
               (push cell seen))
         object)
    (when (and exhaustive? extra)
      (invalid-keys "Unexpected" (map 'list #'car extra)
                    extra))
    (when-let ((missing (remove nil expected :key #'cdr)))
      (invalid-keys "Missing required" (map 'list #'car missing)
                    (list object))))
  object)

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
      (object-error
       (list (list required-version "minimum version declaration" :info))
       "~@<The ~A requires generator version ~S, but this ~
        generator is version ~S.~@:>"
       context required-version generator-version))))

(defun check-name-pathname-congruence (name pathname)
  (unless (string= name (pathname-name pathname))
    (object-error
     (list (list name "name attribute" :error))
     "~@<Value of \"name\" attribute, ~S, does not match filename ~
      ~S.~@:>"
     name (pathname-name pathname)))
  name)

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
       :do (unless (length= 1 cells)
             (object-error
              (loop :for cell :in cells
                    :for i :downfrom (length cells)
                    :collect (list cell (format nil "~:R definition" i)
                                   (if (= i 1) :note :error)))
              "~@<Multiple definitions of variable ~A.~@:>"
              key))
       :collect (value-cons key (cdr (first cells))))))

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
               (let+ (((name &rest versions) included-project))
                 (setf (gethash name projects-seen) included-project)
                 (list (list* name (map 'list #'process-version versions)))))))))
    (make-instance 'distribution-spec
                   :name      name
                   :variables (value-acons :__catalog (lookup :catalog)
                                           (process-variables (lookup :variables)))
                   :versions  (mapcan #'process-project (lookup :versions)))))
