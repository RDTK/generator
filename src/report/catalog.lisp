;;;; catalog.lisp --- Write XML catalog describing recipes.
;;;;
;;;; Copyright (C) 2015-2022 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:build-generator.report)

;;; XML helpers

(defvar *catalog-namespace*
  "https://toolkit.cit-ec.uni-bielefeld.de/CITKat")

(defvar *catalog-image-base-url*
  "https://toolkit.cit-ec.uni-bielefeld.de/sites/toolkit.cit-ec.uni-bielefeld.de/files")

(defun render-person-data? (person style)
  (case (gdpr? style)
    (:opt-in (when (typep person 'project::person)
               (var:as (var:value person :gdpr.opt-in? nil) 'boolean)))
    ((nil)   t)
    ((t)     nil)))

(defvar *output-files*)

(defun call-with-output-to-catalog-file (thunk directory name)
  (ensure-directories-exist directory)
  (let ((filename (make-pathname :name     name
                                 :type     "xml"
                                 :defaults directory)))
    (unwind-protect-case ()
        (with-output-to-file (stream filename #+sbcl :external-format #+sbcl :utf-8
                                              :if-exists :supersede)
          (funcall thunk stream))
      (:normal (pushnew filename *output-files* :test #'equalp))
      (:abort  (ignore-errors (delete-file filename))))))

(defmacro with-output-to-catalog-file ((stream directory name) &body body)
  `(call-with-output-to-catalog-file
    (lambda (,stream) ,@body) ,directory ,name))

(defun call-with-catalog-xml-output (thunk stream &key indentation (gdpr? t))
  (cxml:with-xml-output (cxml:make-character-stream-sink
                         stream :indentation indentation)
    (cxml:processing-instruction "xml-stylesheet" "type=\"text/xsl\" href=\"/static/stylesheet/CITKat.xsl\"")
    (cxml:with-element "catalog"
      (cxml:attribute "xmlns" *catalog-namespace*)
      (when gdpr?
        (cxml:attribute "gdpr" "true"))
      (cxml:attribute "generatorVersion" (asdf:component-version (asdf:find-system :build-generator)))
      (cxml:attribute "creationTime"     (princ-to-string (local-time:now)))
      (funcall thunk))))

(defmacro with-catalog-xml-output ((stream
                                    &key
                                    (indentation nil indentation-supplied?)
                                    (gdpr?       nil gdpr?-supplied?))
                                   &body body)
  `(call-with-catalog-xml-output
    (lambda () ,@body) ,stream
    ,@(when indentation-supplied? `(:indentation ,indentation))
    ,@(when gdpr?-supplied?       `(:gdpr?       ,gdpr?))))

(defun emit-text-element (name value &key (transform #'identity))
  (when value
    (cxml:with-element name (cxml:text (funcall transform value)))))

(defun emit-element-list (list-name element-name items)
  (when items
    (cxml:with-element list-name
      (dolist (item items)
        (cxml:with-element element-name (cxml:text item))))))

(defun emit-resource-element (type value &key name)
  (when value
    (cxml:with-element "resource"
      (when name
        (cxml:attribute "name" name))
      (cxml:attribute "type" type)
      (cxml:attribute "href" value))))

;;; Data access helpers

(defun catalog-value (object key)
  (assoc-value (var:value object :__catalog nil) key))

(define-constant +opensource-schema+
  "opensource://"
  :test #'string=)

(define-constant +catalog-schema+
  "catalog://"
  :test #'string=)

(define-constant +catalog-default-image+
  "catalog://system_image_default.jpg"
  :test #'string=)

(defun catalog-description (object)
  (when-let ((description (catalog-value object :description)))
    (if (starts-with-subseq +opensource-schema+ description)
        (when-let* ((location (project::location-of
                               (typecase object
                                 (project:distribution
                                  object)
                                 (project:version
                                  (model:specification object)))))
                    (source   (text.source-location:source location))
                    (recipe   (text.source-location:name source))
                    (filename (subseq description (length +opensource-schema+)))
                    (path     (reduce #'merge-pathnames
                                      (list
                                       filename
                                       (etypecase object
                                         (project:distribution
                                          "distribution-descriptions/")
                                         (project:version
                                          "project-descriptions/"))
                                       "../assets/"
                                       recipe)))
                    (content  (with-simple-restart
                                  (continue "~@<Skip reading the description ~A~@:>"
                                            path)
                                (read-file-into-string path))))
          content)
        description)))

(defun catalog-images (object)
  (let ((value (catalog-value object :image)))
    (mappend
     (lambda (url)
       (cond
         ((string= url +catalog-default-image+)
          nil)
         ((starts-with-subseq +catalog-schema+ url)
          (list (format nil "~A/~A"
                        *catalog-image-base-url*
                        (subseq url (length +catalog-schema+)))))
         (t
          (list url))))
     (ensure-list value))))

(defun project-version-name (project-version)
  (model:name (model:parent (model:specification project-version))))

(defun project-version-name+version (project-version)
  (format nil "~A-~A"
          (project-version-name project-version)
          (model:name project-version)))

(defun project-version-title (project-version)
  (or (catalog-value project-version :title)
      (project-version-name project-version)))

;;;

(defun emit-description-element (value &key (format "text/plain"))
  (when value
    (cxml:with-element "description"
      (cxml:attribute "format" format)
      (cxml:cdata value))))

(defun emit-generic-metadata (object version filename fallback-name)
  (cxml:attribute "name"    (or (catalog-value object :title) fallback-name))
  (cxml:attribute "version" version)
  (cxml:attribute "access"  (string-downcase (model:access object)))
  (cxml:with-element "filename" (cxml:text filename)))

(defun emit-description (object)
  (or (when-let ((description (catalog-description object)))
        (emit-description-element description :format "text/markdown")
        t)
      (emit-description-element (var:value object :description nil))))

(defun emit-histogram-element (object variable outer-element inner-element)
  (when-let ((values (var:value object variable nil)))
    (cxml:with-element outer-element
      (map nil (lambda+ ((value . count))
                 (cxml:with-element inner-element
                   (cxml:attribute "count" (princ-to-string count))
                   (cxml:text value)))
           values))))

(defun emit-system-dependencies (object platforms)
  (labels ((platform (name version &rest rest-spec)
             (when-let* ((platform (list* name version rest-spec))
                         (requires (with-simple-restart
                                       (continue "~@<Skip platform
                                                  requirements for
                                                  ~{~A~^:~}~@:>"
                                                 platform)
                                     (project:platform-requires object platform))))
               (cxml:with-element "system"
                 (cxml:attribute "name" name)
                 (cxml:attribute "version" version)
                 (cxml:with-element "repository"
                   (cxml:text (format nil "deb http://archive.ubuntu.com/~A ~A multiverse"
                                      name version)))
                 (map nil #'dependency requires))))
           (dependency (dependency)
             (cxml:with-element "dependency"
               (cxml:text dependency))))
    (if-let ((local-platforms (catalog-value object :platforms)))
      (mapcar (lambda (platform)
                (let ((platform (split-sequence:split-sequence
                                 #\Space platform :remove-empty-subseqs t)))
                  (apply #'platform platform)))
              local-platforms)
      (map nil (curry #'apply #'platform) platforms))))

(defun emit-resources (object)
  (let+ (((&flet resource (type variable)
            (emit-resource-element type (var:value object variable nil)))))
    (resource "homepage"      :homepage.url)
    (resource "documentation" :documentation.url)
    (resource "wiki"          :wiki.url)
    (resource "bugtracker"    :bug-tracker.url)
    (resource "scm"           :scm-browser.url)
    (resource "mailing-list"  :mailing-list.url)

    (map nil (curry #'emit-resource-element "img")
         (catalog-images object))))

(defun emit-person-relation (style role person)
  (pushnew person (persons style) :test #'eq)

  (when (render-person-data? person style)
    (let* ((id        (rs.m:name person))
           (unique-id (person-unique-id person)))
      (cxml:with-element "relation"
        (cxml:attribute "type" "person")
        (cxml:attribute "role" role)
        (cxml:attribute "name" id)
        (cxml:text unique-id)))))

(defun emit-relations (style object)
  (flet ((do-role (role title)
           (map nil (curry #'emit-person-relation style title)
                (project:persons-in-role role object))))
    (do-role :recipe.maintainer "Recipe Maintainer")
    (do-role :author            "Author")
    (do-role :maintainer        "Maintainer")
    (do-role :committer         "Committer")))

(defun emit-direct-dependency (name version href)
  (cxml:with-element "directDependency"
    (cxml:attribute "name"    name)
    (cxml:attribute "version" version)
    (cxml:text (util:safe-name href))))

(defun project-version-dependency (project-version)
  (let ((title   (project-version-title project-version))
        (version (model:name project-version)))
    (emit-direct-dependency
     title version (project-version-name+version project-version))))

;;; Report class and methods

(deftype person-style ()
  `(member nil t))

(defclass catalog ()
  ((indentation  :initarg  :indentation
                 :reader   indentation
                 :initform nil)
   (gdpr?        :initarg  :gdpr?
                 :reader   gdpr?
                 :initform nil)
   (person-style :initarg  :person-style
                 :type     person-style
                 :reader   person-style
                 :initform nil)
   (persons      :accessor persons
                 :initform '())
   (platforms    :initarg  :platforms
                 :reader   platforms
                 :initform '(("ubuntu" "trusty" "x86_64")
                             ("ubuntu" "xenial" "x86_64")
                             ("ubuntu" "bionic" "x86_64")))))

(defmethod report :around ((object t) (style catalog) (target stream))
  (with-condition-translation (((error report-error)
                                :style  style
                                :object object))
    (call-next-method)))

(defmethod report ((object sequence) (style catalog) (target pathname))
  (map nil (lambda (element)
             (with-simple-restart (continue "~@<Skip ~A~@:>" element)
               (report element style target)))
       object))

(defmethod report ((object project:distribution)
                   (style  catalog)
                   (target pathname))
  (let ((distribution-directory (merge-pathnames #P"distribution/" target)))
    (with-output-to-catalog-file (stream distribution-directory
                                         (util:safe-name (model:name object)))
      (report object style stream)))

  (let ((project-directory (merge-pathnames #P"project/" target)))
    (report (project:versions object) style project-directory)))

(defmethod report ((object project:distribution)
                   (style  catalog)
                   (target stream))
  (with-catalog-xml-output (target :indentation (indentation style)
                                   :gdpr?       (gdpr? style))
    (cxml:with-element "distribution"
      ;; Generic metadata
      (let ((name    (util:safe-name (model:name object)))
            (version (catalog-value object :version)))
        (emit-generic-metadata object version name name))

      ;; Description
      (emit-description object)

      ;; Natures, Languages, licenses
      (emit-histogram-element object :natures "natures" "nature")
      (emit-histogram-element object :programming-languages
                              "programmingLanguages" "language")
      (emit-histogram-element object :licenses "licenses" "license")

      ;; Dependencies
      (cxml:with-element "dependencies"
        ;; System dependencies
        (emit-system-dependencies object (platforms style))
        ;; Included projects
        (map nil #'project-version-dependency (project:versions object)))

      (when-let ((replication (catalog-value object :replication)))
        (cxml:with-element "replication" (cxml:text replication)))
      (when-let ((message (var:value object :message nil)))
        (cxml:with-element "message" (cxml:text message)))

      ;; Resources
      (emit-resources object)

      ;; Relations
      (emit-relations style object))))

(defmethod report ((object project:version)
                   (style  catalog)
                   (target pathname))
  (with-output-to-catalog-file
      (stream target (util:safe-name (project-version-name+version object)))
    (report object style stream)))

(defmethod report ((object project:version)
                   (style  catalog)
                   (target stream))
  (with-catalog-xml-output (target :indentation (indentation style)
                                   :gdpr?       (gdpr? style))
    (cxml:with-element "project"
      ;; Generic metadata
      (let ((name          (model:name object))
            (safe-name     (util:safe-name (project-version-name+version object)))
            (fallback-name (project-version-name object)))
        (emit-generic-metadata object name safe-name fallback-name))

      ;; Description and keywords
      (emit-description object)
      (emit-element-list
       "keywords" "keyword" (var:value/cast object :keywords '()))
      (let ((licenses (or (var:value object :licenses nil)
                          (ensure-list (var:value object :license nil))
                          (ensure-list (var:value object :analysis.license nil)))))
        (cxml:with-element "licenses"
          (map nil (curry #'emit-text-element "license") licenses)))

      ;; Repository
      (cxml:with-element "scm"
        (emit-text-element "kind"          (var:value object :analysis.scm nil)
                                           :transform #'string-downcase)
        (emit-text-element "repository"    (var:value object :repository nil))
        (emit-text-element "sub-directory" (var:value object :sub-directory nil))
        (when-let ((id   (var:value object :analysis.most-recent-commit.id   nil))
                   (date (var:value object :analysis.most-recent-commit.date nil)))
          (cxml:with-element "revision"
            (emit-text-element "id"   id)
            (emit-text-element "date" date))))

      ;; Nature and languages
      (emit-element-list
       "natures" "nature" (var:value object :analysis.natures '()))
      (emit-element-list
       "programmingLanguages" "language"
       (var:value object :analysis.programming-languages '()))

      ;; Dependencies
      (cxml:with-element "dependencies"
        ;; System dependencies
        (emit-system-dependencies object (platforms style))

        ;; Project dependencies.
        (map nil #'project-version-dependency (model:direct-dependencies object)))

      ;; Resources
      (emit-resources object)

      ;; Relations
      (emit-relations style (model:specification object)))))

(defmethod report ((object rosetta-project.model.resource:person)
                   (style  catalog)
                   (target pathname))
  (when (eq (gdpr? style) t)
    (with-output-to-catalog-file (stream target (person-unique-id object))
      (report object style stream))))

(defmethod report ((object project::person)
                   (style  catalog)
                   (target pathname))
  (when (render-person-data? object style)
    (with-output-to-catalog-file (stream target (person-unique-id object))
      (report object style stream))))

(defun person-uri (person)
  (first (rosetta-project.model.resource:identities person)))

(defun person-unique-id (person)
  (if-let ((uri (person-uri person)))
    (puri:uri-path uri)
    (substitute-if-not #\_ #'alphanumericp
                       (string-downcase (rs.m:name person)))))

(defun person-gravatar (person)
  (when-let* ((email (person-uri person))
              (path  (puri:uri-path email))
              (hash  (ironclad:digest-sequence
                      :md5 (sb-ext:string-to-octets path))))
    (format nil "~(~{~2,'0X~}~)" (coerce hash 'list))))

(defmethod report ((object rosetta-project.model.resource:person)
                   (style  catalog)
                   (target stream))
  (let+ ((names      (rosetta-project.model.resource:names object))
         (identities (rosetta-project.model.resource:identities object))
         (identity   (person-unique-id object))
         ((&values provider id) (split-identity identity)))
    (with-catalog-xml-output (target :indentation (indentation style)
                                     :gdpr?       (gdpr? style))
      (cxml:with-element "person"
        (cxml:attribute "provider" provider)
        (cxml:attribute "pid"      id)
        (cxml:attribute "url"      identity)

        (emit-text-element "filename" identity)

        (emit-text-element "gravatar" (person-gravatar object))

        (map nil (curry #'emit-text-element "name") names)

        (map nil (lambda (identity)
                   (when (rosetta-project.model.resource:score-better
                          (rosetta-project.model.resource:score-email-address
                           identity)
                          :non-functional)
                     (emit-text-element "email" identity
                                        :transform #'princ-to-string)))
             identities)))))

;;; Entry point

(defmethod report ((object t) (style (eql :catalog)) (target t))
  (destructuring-bind (target . platforms) target ; HACK
    (let ((style  (apply #'make-instance 'catalog
                         :gdpr? :opt-in
                         (when platforms (list :platforms platforms))))
          (object (if (and (typep object '(and sequence (not null)))
                           (typep (first-elt object) 'project:distribution))
                      (make-instance 'distributions :distributions object)
                      object)))
      (report object style target))))

(defclass distributions ()
  ((distributions :initarg :distributions
                  :reader  distributions)))

(defmethod report :around ((object distributions)
                           (style  catalog)
                           (target pathname))
  (let ((*output-files* '()))
    (call-next-method)))

(defmethod report ((object distributions)
                   (style  catalog)
                   (target t))
  (report (distributions object) style target))

(defmethod report :after ((object distributions)
                          (style  catalog)
                          (target pathname))
  (let ((person-directory (merge-pathnames #P"person/" target)))
    (map nil (rcurry #'report style person-directory)
         (persons style)))

  ;; Write names (relative to TARGET, without type component) of
  ;; output files into index.json.
  (with-output-to-file (stream (merge-pathnames #P"index.json" target)
                               #+sbcl :external-format #+sbcl :utf-8
                               :if-exists :supersede)
    (json:encode-json
     (map 'vector (lambda (filename)
                    (enough-namestring (make-pathname :type     nil
                                                      :defaults filename)
                                       target))
          *output-files*)
     stream)))

;;; Utilities

(defun split-identity (identity)
  (let ((index (position #\@ identity)))
    (values (subseq identity (1+ (or index -1)))
            (subseq identity 0                 (or index (length identity))))))
