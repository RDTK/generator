;;;; functions-input.lisp --- Functions for loading recipes.
;;;;
;;;; Copyright (C) 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.project.commands)

(defun collect-inputs (spec)
  (cond
    ((wild-pathname-p spec)
     (directory spec))
    ((pathnamep spec)
     (unless (probe-file spec)
       (error "~@<Input file does not exist: ~S.~@:>" spec))
     (list spec))
    (t
     (error "~@<Invalid input specification: ~S.~@:>" spec))))

(defun locate-specifications (kind patterns &key (if-no-match #'error))
  (with-simple-restart (continue "~@<Do not load ~A specifications.~@:>" kind)
    (or (iter (for pattern in patterns)
              (when-let ((matches (collect-inputs (pathname pattern))))
                (appending matches)))
        (error-behavior-restart-case
            (if-no-match (simple-error
                          :format-control   "~@<None of the ~A patterns ~
                                             ~{~S~^, ~} matched ~
                                             anything.~@:>"
                          :format-arguments (list kind patterns))
                         :warning-condition simple-warning)))))

(defun load-specifications (kind files loader)
  (let ((name (let ((*package* (find-package :keyword)))
                (symbolicate '#:load/ kind))))
    (with-sequence-progress (name files)
      (mapcan (lambda (file)
                (progress "~S" file)
                (with-simple-restart
                    (continue "~@<Skip ~A specification ~S.~@:>" kind file)
                  (list (funcall loader file))))
              files))))

;;; Templates

(defun derive-template-pattern (distribution mode)
  (merge-pathnames (make-pathname
                    :name      :wild
                    :type      "template"
                    :directory `(:relative :back "templates" ,mode))
                   distribution))

;;; Projects

(defun derive-project-pattern (distribution name)
  (merge-pathnames (make-pathname
                    :name      name
                    :type      "project"
                    :directory '(:relative :back "projects"))
                   distribution))

(defun locate-projects (distribution-pathnames distributions)
  (let+ ((projects (make-hash-table :test #'equalp))
         ((&flet ensure-project (location versions distribution)
            (if-let ((project (gethash location projects)))
              (unionf (second project) versions :test #'equal)
              (setf (gethash location projects)
                    (list location versions distribution))))))
    (map nil (lambda (distribution-pathname distribution)
               (map nil (lambda+ ((name &rest versions))
                          (when-let* ((pattern  (derive-project-pattern
                                                 distribution-pathname name))
                                      (location (first (locate-specifications
                                                        :project (list pattern)))))
                            (ensure-project location versions distribution)))
                    (jenkins.model.project:versions distribution)))
         distribution-pathnames distributions)
    (hash-table-values projects)))

(defstruct project-spec-and-versions
  (spec     nil :type jenkins.model.project::project-spec :read-only t)
  (versions nil :type list                                :read-only t))

(defmethod print-items:print-items append ((object project-spec-and-versions))
  (let+ (((&structure-r/o project-spec-and-versions- spec versions) object)
         (versions (map 'list #'name versions)))
    (append (print-items:print-items spec)
            `((:versions ,versions ":~{~A~^,~}" ((:after :name)))))))

(defun load-project/versioned (file versions distribution &key generator-version)
  (let+ ((version-names (mapcar #'first versions))
         (project       (reinitialize-instance
                         (load-project-spec/json
                          file
                          :version-test      (lambda (version)
                                               (find version version-names
                                                     :test #'string=))
                          :generator-version generator-version)
                         :parent distribution))
         (branches      (value/cast project :branches '()))
         (branches      (intersection version-names branches :test #'string=))
         (tags          (value/cast project :tags '()))
         (tags          (intersection version-names tags :test #'string=))
         (tags+branches (union branches tags))
         (versions1     (set-difference version-names tags+branches
                                        :test #'string=))
         ((&flet process-version (name &key version-required? branch? tag?)
            (with-simple-restart (continue "~@<Skip version ~S.~@:>" name)
              (let* ((parameters    (second (find name versions
                                                  :test #'string=
                                                  :key  #'first)))
                     (version       (cond
                                      ((find name (versions project)
                                             :test #'string= :key #'name))
                                      (version-required?
                                       (error "~@<No version section for ~
                                               version ~S in project ~
                                               ~A.~@:>"
                                              name project))
                                      (t
                                       (make-instance 'version-spec
                                                      :name      name
                                                      :parent    project
                                                      :variables '()))))
                     (name-variable (cond
                                      (branch? :branch)
                                      (tag?    :tag)))
                     (variables     (append
                                     parameters
                                     (when (and name-variable
                                                (not (jenkins.model.variables:value
                                                      version name-variable nil)))
                                       (list (value-cons name-variable name)))
                                     (jenkins.model.variables::%direct-variables
                                      version))))
                (list (reinitialize-instance version :variables variables))))))
         (versions (append (mapcan (rcurry #'process-version :branch? t)
                                   branches)
                           (mapcan (rcurry #'process-version :tag? t)
                                   tags)
                           (mapcan (rcurry #'process-version :version-required? t)
                                   versions1))))
    (make-project-spec-and-versions
     :spec     (reinitialize-instance project :versions versions)
     :versions versions)))

(defun load-projects/versioned (files-and-versions &key generator-version)
  (with-sequence-progress (:load/project files-and-versions)
    (lparallel:pmapcan
     (lambda+ ((file versions distribution))
       (progress "~A" file)
       (with-simple-restart
           (continue "~@<Skip project specification ~S.~@:>" file)
         (list (load-project/versioned file versions distribution
                                       :generator-version generator-version))))
     :parts most-positive-fixnum files-and-versions)))

;;; The values of these variables uniquely identify a "repository
;;; access".
;;;
;;; Project versions can be grouped according to the values of these
;;; variables and analyzed together.
(defvar *repository-variables*
  '(:repository :scm :scm.username :scm.password :sub-directory))

(defun group-project-versions-for-analysis (project)
  (let+ (((&structure-r/o project-spec-and-versions- versions) project)
         ((&flet maybe-key-fragment (version variable)
            (when-let ((value (jenkins.model.variables:value
                               version variable nil)))
              (list variable value))))
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
              (value-cons name (to-value value)))))
         ((&flet person->string (person)
            (format nil "~A~@[ <~A>~]"
                    (rs.m:name person)
                    (first (rosetta-project.model.resource:identities person))))))
    (iter (for (key value) :on results :by #'cddr)
          (case key
            ((:authors :maintainers :committers :recipe.maintainers)
             (let ((persons (ensure-persons! value)))
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

(defun analyze-version (version results)
  (let+ (((&plist-r/o (scm              :scm)
                      (branch-directory :branch-directory)
                      (requires         :requires)
                      (provides         :provides))
          results)
         (other-results      (remove-from-plist results
                                                :requires :provides
                                                :properties))
         (recipe-maintainers (jenkins.analysis::parse-people-list
                              (jenkins.model.variables:value
                               version :recipe.maintainer '())))
         ((&values analysis-variables persons)
          (make-analysis-variables
           (list* :recipe.maintainers recipe-maintainers
                  other-results))))
    (reinitialize-instance
     version
     :requires  requires
     :provides  provides
     :variables (append
                 (jenkins.model.variables:direct-variables version)
                 (when scm
                   (list (value-cons :scm (string-downcase scm))))
                 (when branch-directory
                   (list (value-cons :branch-directory branch-directory)))
                 analysis-variables)
     :persons   persons)))

;;; In combination with values of the variables in
;;; `*repository-variables*', the named variables uniquely identify a
;;; the input data of a project analyses process.
(defvar *analysis-variables*
  '(:branch :tag :commit :directory :natures))

(defun analyze-project (project &rest args &key cache-directory temp-directory non-interactive)
  (declare (ignore cache-directory temp-directory non-interactive))
  (let+ ((groups (group-project-versions-for-analysis project))
         ((&flet version-info (version)
            (let+ (((&flet maybe-key-fragment (version variable)
                      (when-let ((value (jenkins.model.variables:value
                                         version variable nil)))
                        (list variable value))))
                   (info    (mapcan (curry #'maybe-key-fragment version)
                                    *analysis-variables*))
                   (natures (when-let ((natures (getf info :natures)))
                              (list :natures (map 'list (compose #'make-keyword
                                                                 #'string-upcase)
                                                  natures)))))
              (append natures (remove-from-plist info :natures)))))
         ((&flet+ analyze-group ((info . versions))
            (let+ (((&plist-r/o (repository :repository)) info)
                   (uri        (when repository (puri:uri repository)))
                   (other-info (remove-from-plist info :repository)))
              (mapcan
               (lambda (version results)
                 (when results
                   (list (analyze-version version results))))
               versions
               (apply #'jenkins.analysis:analyze uri :auto
                      :versions (map 'list #'version-info versions)
                      (append other-info args)))))))
    (mapc #'analyze-group groups)
    (project-spec-and-versions-spec project)))

(defun analyze-projects (projects
                         &rest args &key
                         generator-version
                         cache-directory
                         temp-directory
                         non-interactive)
  (declare (ignore cache-directory temp-directory non-interactive))
  (jenkins.analysis::with-git-cache ()
    (let ((other-args (remove-from-plist args :generator-version))
          (cache      jenkins.analysis::*git-cache*))
      (with-sequence-progress (:analyze/project projects)
        (lparallel:pmapcan
         (lambda (project)
           (progress "~/print-items:format-print-items/"
                     (print-items:print-items project))
           (more-conditions::without-progress
             (let ((jenkins.analysis::*git-cache*     cache)
                   (jenkins.analysis::*cache-version* generator-version))
               (with-simple-restart
                   (continue "~@<Skip analyzing project ~A.~@:>" project)
                 (when-let ((project (apply #'analyze-project project
                                            other-args)))
                   (list (setf (find-project (name project)) project)))))))
         :parts most-positive-fixnum projects)))))

(defun resolve-project-version (project version)
  (let ((project (find-project project)))
    (or (find version (versions project) :test #'string= :key #'name)
        (error "~@<Could not find version ~S in project ~A.~@:>"
               version project))))

(defun resolve-project-versions (versions)
  (mapcan (lambda+ ((project &rest versions))
            (mapcan (lambda+ ((version &optional &ign))
                      (with-simple-restart
                          (continue "~@<Skip version ~A of project ~A.~@:>"
                                    version project)
                        (list (resolve-project-version project version))))
                    versions))
          versions))

;;; Distributions

(defun set-overwrites (distributions overwrites)
  (with-sequence-progress (:overwrites distributions)
   (map (class-of distributions)
        (lambda (distribution)
          (progress "~S" distribution)
          (with-simple-restart
              (continue "~@<Do not overwrite variables in ~S.~@:>"
                        distribution)
            (iter (for (name . value) in overwrites)
                  (log:info "~@<In ~A, setting ~S to ~S.~@:>"
                            distribution name value)
                  (setf (lookup distribution name) value)))
          distribution)
        distributions)))
