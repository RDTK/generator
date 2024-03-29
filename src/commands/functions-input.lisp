;;;; functions-input.lisp --- Functions for loading recipes.
;;;;
;;;; Copyright (C) 2017-2022 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:build-generator.commands)

(defun collect-inputs (spec)
  (cond
    ((wild-pathname-p spec)
     (directory spec))
    ((pathnamep spec)
     (if-let ((truename (probe-file spec)))
       (list truename)
       (project::object-error
        (list (list spec "included here" :error))
        "~@<File does not exist: ~A.~@:>" spec)))
    (t
     (error "~@<Invalid input specification: ~S.~@:>" spec))))

(defun locate-specifications (kind patterns repository &key (if-no-match #'error))
  (with-simple-restart (continue "~@<Do not load ~A specifications.~@:>" kind)
    (or (iter (for pattern in patterns)
              (when-let ((matches (collect-inputs (project:recipe-truename repository kind pattern))))
                (appending matches)))
        (error-behavior-restart-case
            (if-no-match (simple-error
                          :format-control   "~@<None of the ~A patterns ~
                                             ~{~S~^, ~} matched ~
                                             anything.~@:>"
                          :format-arguments (list kind patterns))
                         :warning-condition simple-warning)))))

(defun load-specifications (kind files loader repository)
  (let ((name (let ((*package* (find-package :keyword)))
                (symbolicate '#:load/ kind))))
    (with-sequence-progress (name files)
      (mapcan (lambda (file)
                (progress "~A" (util:safe-enough-namestring file))
                (with-simple-restart
                    (continue "~@<Skip ~A specification ~S.~@:>" kind file)
                  (list (funcall loader file :repository repository))))
              files))))

(defun derive-root-repository (distribution-pathname mode
                               &key cache-directory)
  (let ((root-directory (merge-pathnames
                         (make-pathname :directory '(:relative :back))
                         (uiop:pathname-directory-pathname distribution-pathname))))
    (project:load-repository root-directory mode
                             :cache-directory cache-directory)))

;;; Projects

(defun locate-projects (distributions repository)
  (let+ ((projects (make-hash-table :test #'equalp))
         ((&flet ensure-project (location project-include)
            (if-let ((project (gethash location projects)))
              (pushnew project-include (second project)
                       :test #'string= :key #'project:version) ; TODO should consider parameters as part of the key
              (setf (gethash location projects)
                    (list location (list project-include)))))))
    (map nil (lambda (distribution)
               (map nil (lambda (project-include)
                          (let ((name (project:project project-include)))
                            (when-let* ((pattern  (project::copy-location
                                                   name (project:recipe-truename repository :project name)))
                                        (location (first (locate-specifications
                                                          :project (list pattern) repository))))
                              (ensure-project location project-include))))
                    (project:versions distribution)))
         distributions)
    (hash-table-values projects)))

(defun load-project/versioned (file version-names repository
                               &key generator-version)
  (let+ ((project       (project:load-project-spec/yaml
                         file
                         :repository        repository
                         :version-test      (lambda (name pattern)
                                              (cond
                                                (name
                                                 (find name version-names
                                                       :test #'string=))
                                                (pattern
                                                 (remove pattern version-names
                                                         :test-not #'ppcre:scan))))
                         :generator-version generator-version))
         (branches      (var:value/cast project :branches '()))
         (branches      (intersection version-names branches :test #'string=))
         (tags          (var:value/cast project :tags '()))
         (tags          (set-difference tags branches :test #'string=))
         (tags          (intersection version-names tags :test #'string=))
         (tags+branches (union branches tags :test #'string=))
         (versions1     (set-difference version-names tags+branches
                                        :test #'string=))
         ((&flet process-version (name &key version-required? branch? tag?)
            (with-simple-restart (continue "~@<Skip version ~S.~@:>" name)
              (let* ((version       (cond
                                      ((find name (project:versions project)
                                             :test #'string= :key #'model:name))
                                      (version-required?
                                       (project::object-error
                                        (list (list name "included here" :error))
                                        "~@<No version section for ~
                                         version ~S in project ~
                                         ~/print-items:format-print-items/~
                                         .~@:>"
                                        name (print-items:print-items project)))
                                      (t
                                       (let ((version-spec
                                               (make-instance 'project::version-spec
                                                              :name      name
                                                              :parent    project
                                                              :variables '())))
                                         (project::copy-location
                                          name version-spec)))))
                     (name-variable (cond
                                      (branch? :branch)
                                      (tag?    :tag))))
                (when (and name-variable
                           (not (var:value version name-variable nil)))
                  (setf (var:lookup version name-variable) name))
                (list version)))))
         (versions (append (mapcan (rcurry #'process-version :branch? t)
                                   branches)
                           (mapcan (rcurry #'process-version :tag? t)
                                   tags)
                           (mapcan (rcurry #'process-version :version-required? t)
                                   versions1))))
    (reinitialize-instance project :versions versions)))

(defun load-projects/versioned (files-and-includes repository &key generator-version)
  (with-sequence-progress (:load/project files-and-includes)
    (lparallel:pmapcan
     (lambda+ ((file project-includes))
       (progress "~A" (util:safe-enough-namestring file))
       (with-simple-restart
           (continue "~@<Skip project specification ~S.~@:>" file)
         (let ((version-names (map 'list #'project:version project-includes)))
          (list (load-project/versioned
                 file version-names repository
                 :generator-version generator-version)))))
     :parts most-positive-fixnum files-and-includes)))

;;; The values of these variables uniquely identify a "repository
;;; access".
;;;
;;; Project versions can be grouped according to the values of these
;;; variables and analyzed together.
(defvar *repository-variables*
  '(:repository
    :scm
    :scm.username
    :scm.password
    (:sub-directory . uiop:ensure-directory-pathname)))

(defun group-project-versions-for-analysis (project)
  (let+ ((versions (project:versions project))
         ((&flet maybe-key-fragment (version variable-and-transform)
            (let+ (((variable . transform)
                    (ensure-list variable-and-transform)))
              (when-let ((value (var:value version variable nil)))
                (list variable (if transform
                                   (funcall transform value)
                                   value))))))
         ((&flet version-analysis-data (version)
            (values version
                    (mapcan (curry #'maybe-key-fragment version)
                            *repository-variables*))))
         (groups (make-hash-table :test #'equalp)))
    (map nil (lambda (version)
               (let+ (((&values proto-version key)
                       (version-analysis-data version)))
                 (push proto-version (gethash key groups '()))))
         versions)
    (hash-table-alist groups)))

(defun make-analysis-variables (results)
  (let+ (((&flet make-variable (key value)
            (let ((name (format-symbol '#:keyword "ANALYSIS.~A" key)))
              (var:value-cons name (var:to-value value)))))
         ((&flet person->string (person)
            (format nil "~A~@[ <~A>~]"
                    (rs.m:name person)
                    (first (rosetta-project.model.resource:identities person))))))
    (iter (for (key value) :on results :by #'cddr)
          (case key
            ((:authors :maintainers :committers :recipe.maintainers)
             (let ((persons (project:ensure-persons! value)))
               (collect (ecase key
                          (:authors            :author)
                          (:maintainers        :maintainer)
                          (:committers         :committer)
                          (:recipe.maintainers :recipe.maintainer))
                 :into people)
               (collect persons :into people)
               (collect (make-variable key (map 'list #'person->string persons))
                 :into variables)))
            (t
             (collect (make-variable key value) :into variables)))
          (finally (return (values variables people))))))

(defun add-analysis-results-to-version (version results)
  (let+ (((&plist-r/o (scm              :scm)
                      (branch-directory :branch-directory)
                      (requires         :requires)
                      (provides         :provides))
          results)
         (other-results      (remove-from-plist results
                                                :requires :provides
                                                :properties))
         (recipe-maintainers (analysis::parse-people-list
                              (var:value version :recipe.maintainer '())))
         ((&values analysis-variables persons)
          (make-analysis-variables
           (list* :recipe.maintainers recipe-maintainers
                  other-results))))
    (reinitialize-instance
     version
     :requires  requires
     :provides  provides
     :variables (append
                 (var:direct-variables version)
                 (when scm
                   (list (var:value-cons :scm (string-downcase scm))))
                 (when branch-directory
                   (list (var:value-cons :branch-directory branch-directory)))
                 analysis-variables)
     :persons   persons)))

;;; In combination with values of the variables in
;;; `*repository-variables*', the named variables uniquely identify a
;;; the input data of a project analyses process.
(defvar *analysis-variables*
  `(:branch
    :tag
    :commit
    :directory
    (:natures . ,(lambda (natures)
                   (map 'list (compose #'make-keyword
                                       #'string-upcase)
                        natures)))))

(defun resolve-analysis-variables (version)
  ;; Return a plist of the values in VERSION of the variables in
  ;; *ANALYSIS-VARIABLES*. The plist entries are in the order defined
  ;; by *ANALYSIS-VARIABLES* which is important since these lists are
  ;; de-duplicated using EQUAL in ANALYZE-PROJECT.
  (flet ((maybe-key-fragment (variable-and-transform)
           (let+ (((variable . transform)
                   (ensure-list variable-and-transform)))
             (when-let ((value (var:value version variable nil)))
               (list variable (if transform
                                  (funcall transform value)
                                  value))))))
    (mapcan #'maybe-key-fragment *analysis-variables*)))

(defun analyze-project (project &rest args
                                &key temp-directory
                                     cache-directory
                                     age-limit)
  (declare (ignore temp-directory cache-directory age-limit))
  (let+ ((groups (group-project-versions-for-analysis project))
         ((&flet+ compute-version-infos (versions)
            ;; Within a group, versions can still produce identical
            ;; repository information lists (for example when
            ;; different versions use the same branch but differ in
            ;; some non-version-control parameters). To address this
            ;; issue, map versions to unique repository information
            ;; lists, then map results back to versions.
            (let ((infos         (make-hash-table :test #'equal))
                  (version->info (make-hash-table :test #'eq)))
              (map nil (lambda (version)
                         (let* ((info        (resolve-analysis-variables
                                              version))
                                (unique-info (ensure-gethash
                                              info infos info)))
                           (setf (gethash version version->info) unique-info)
                           info))
                   versions)
              (values (hash-table-values infos) version->info))))
         ((&flet+ analyze-group ((info . versions))
            (let+ (((&plist-r/o (repository :repository)
                                (username   :scm.username)
                                (password   :scm.password))
                    info)
                   (other-info (append
                                (remove-from-plist
                                 info :repository :scm.username :scm.password)
                                (when username (list :username username))
                                (when password (list :password password))))
                   ((&values version-infos version->info)
                    (compute-version-infos versions))
                   (results (apply #'analysis:analyze repository :auto
                                   :project  project
                                   :versions version-infos
                                   (append other-info args))))
              (map nil (lambda (version)
                         (when-let* ((info   (gethash version version->info))
                                     (index  (position info version-infos))
                                     (result (nth index results)))
                           (add-analysis-results-to-version version result)))
                   versions)))))
    (map nil #'analyze-group groups)
    project))

(defun analyze-projects (projects &rest args
                                  &key generator-version
                                       temp-directory
                                       cache-directory
                                       age-limit)
  (declare (ignore temp-directory cache-directory age-limit))
  (analysis::with-git-cache ()
    (let ((other-args (remove-from-plist args :generator-version))
          (cache      analysis::*git-cache*))
      (with-sequence-progress (:analyze/project projects)
        (lparallel:pmapcan
         (lambda (project)
           (progress "~/print-items:format-print-items/"
                     (print-items:print-items project))
           (more-conditions::without-progress
             (let ((analysis::*git-cache*     cache)
                   (analysis::*cache-version* generator-version))
               (with-simple-restart
                   (continue "~@<Skip analyzing project ~A.~@:>" project)
                 (when-let ((project (apply #'analyze-project project
                                            other-args)))
                   (list (setf (project:find-project (model:name project)) project)))))))
         :parts most-positive-fixnum projects)))))

(defun resolve-project-version (project version)
  (let ((project (project:find-project project)))
    (or (find version (project:versions project) :test #'string= :key #'model:name)
        (error "~@<Could not find version ~S in project ~
                ~/print-items:format-print-items/.~@:>"
               version (print-items:print-items project)))))

(defun resolve-project-versions (versions)
  (mapcan (lambda (project-include)
            (let ((project    (project:project project-include))
                  (version    (project:version project-include))
                  (parameters (var:direct-variables project-include)))
              (with-simple-restart
                  (continue "~@<Skip version ~A of project ~A.~@:>"
                            version project)
                (list (make-instance 'project:resolved-project-include
                                     :specification project-include
                                     :version       (resolve-project-version
                                                     project version)
                                     :variables     parameters)))))
          versions))

;;; Distributions

(defun parse-distribution-persons (distributions)
  (map (class-of distributions)
       (lambda (distribution)
         (with-simple-restart
             (continue "~@<Do not overwrite variables in ~S.~@:>"
                       distribution)
           (let ((recipe-maintainers
                   (project:ensure-persons!
                    (analysis::parse-people-list
                     (var:value/cast distribution :recipe.maintainer '())))))
             (reinitialize-instance
              distribution :persons `(:recipe.maintainer ,recipe-maintainers))
             distribution)))
       distributions))
