;;;; functions-input.lisp --- Functions for loading recipes.
;;;;
;;;; Copyright (C) 2017, 2018, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.project.commands)

(defun collect-inputs (spec)
  (cond
    ((wild-pathname-p spec)
     (directory spec))
    ((pathnamep spec)
     (unless (probe-file spec)
       (jenkins.model.project::object-error
        (list (list spec "included here" :error))
        "~@<File does not exist: ~A.~@:>" spec))
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
                (progress "~A" (jenkins.util:safe-enough-namestring file))
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
  (jenkins.model.project::copy-location
   name (merge-pathnames (make-pathname
                          :name      name
                          :type      "project"
                          :directory '(:relative :back "projects"))
                         distribution)))

(defun locate-projects (distribution-pathnames distributions)
  (let+ ((projects (make-hash-table :test #'equalp))
         ((&flet ensure-project (location project-include)
            (if-let ((project (gethash location projects)))
              (pushnew project-include (second project)
                       :test #'string= :key #'jenkins.model.project:version) ; TODO should consider parameters as part of the key
              (setf (gethash location projects)
                    (list location (list project-include)))))))
    (map nil (lambda (distribution-pathname distribution)
               (map nil (lambda (project-include)
                          (let ((name (project project-include)))
                            (when-let* ((pattern  (derive-project-pattern
                                                   distribution-pathname name))
                                        (location (first (locate-specifications
                                                          :project (list pattern)))))
                              (ensure-project location project-include))))
                    (versions distribution)))
         distribution-pathnames distributions)
    (hash-table-values projects)))

(defun load-project/versioned (file version-names &key generator-version)
  (let+ ((project       (load-project-spec/yaml
                         file
                         :version-test      (lambda (name pattern)
                                              (cond
                                                (name
                                                 (find name version-names
                                                       :test #'string=))
                                                (pattern
                                                 (remove pattern version-names
                                                         :test-not #'ppcre:scan))))
                         :generator-version generator-version))
         (branches      (value/cast project :branches '()))
         (branches      (intersection version-names branches :test #'string=))
         (tags          (value/cast project :tags '()))
         (tags          (set-difference tags branches :test #'string=))
         (tags          (intersection version-names tags :test #'string=))
         (tags+branches (union branches tags :test #'string=))
         (versions1     (set-difference version-names tags+branches
                                        :test #'string=))
         ((&flet process-version (name &key version-required? branch? tag?)
            (with-simple-restart (continue "~@<Skip version ~S.~@:>" name)
              (let* ((version       (cond
                                      ((find name (versions project)
                                             :test #'string= :key #'name))
                                      (version-required?
                                       (jenkins.model.project::object-error
                                        (list (list name "included here" :error))
                                        "~@<No version section for ~
                                         version ~S in project ~
                                         ~/print-items:format-print-items/~
                                         .~@:>"
                                        name (print-items:print-items project)))
                                      (t
                                       (let ((version-spec
                                               (make-instance 'version-spec
                                                              :name      name
                                                              :parent    project
                                                              :variables '())))
                                         (jenkins.model.project::copy-location
                                          name version-spec)))))
                     (name-variable (cond
                                      (branch? :branch)
                                      (tag?    :tag))))
                (when (and name-variable
                           (not (jenkins.model.variables:value
                                 version name-variable nil)))
                  (setf (jenkins.model.variables:lookup version name-variable) name))
                (list version)))))
         (versions (append (mapcan (rcurry #'process-version :branch? t)
                                   branches)
                           (mapcan (rcurry #'process-version :tag? t)
                                   tags)
                           (mapcan (rcurry #'process-version :version-required? t)
                                   versions1))))
    (reinitialize-instance project :versions versions)))

(defun load-projects/versioned (files-and-includes &key generator-version)
  (with-sequence-progress (:load/project files-and-includes)
    (lparallel:pmapcan
     (lambda+ ((file project-includes))
       (progress "~A" (jenkins.util:safe-enough-namestring file))
       (with-simple-restart
           (continue "~@<Skip project specification ~S.~@:>" file)
         (let ((version-names (map 'list #'jenkins.model.project:version
                                   project-includes)))
          (list (load-project/versioned
                 file version-names :generator-version generator-version)))))
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
  (let+ ((versions (versions project))
         ((&flet maybe-key-fragment (version variable-and-transform)
            (let+ (((variable . transform)
                    (ensure-list variable-and-transform)))
              (when-let ((value (jenkins.model.variables:value
                                 version variable nil)))
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
  `(:branch
    :tag
    :commit
    :directory
    (:natures . ,(lambda (natures)
                   (map 'list (compose #'make-keyword
                                       #'string-upcase)
                        natures)))))

(defun analyze-project (project &rest args &key cache-directory temp-directory non-interactive)
  (declare (ignore cache-directory temp-directory non-interactive))
  (let+ ((groups (group-project-versions-for-analysis project))
         ((&flet version-info (version)
            (let+ (((&flet maybe-key-fragment (version variable-and-transform)
                      (let+ (((variable . transform)
                              (ensure-list variable-and-transform)))
                            (when-let ((value (jenkins.model.variables:value
                                               version variable nil)))
                              (list variable (if transform
                                                 (funcall transform value)
                                                 value)))))))
              (mapcan (curry #'maybe-key-fragment version)
                      *analysis-variables*))))
         ((&flet+ analyze-group ((info . versions))
            (let+ (((&plist-r/o (repository :repository)) info)
                   (other-info (remove-from-plist info :repository)))
              (mapcan
               (lambda (version results)
                 (when results
                   (list (analyze-version version results))))
               versions
               (apply #'jenkins.analysis:analyze repository :auto
                      :project  project
                      :versions (map 'list #'version-info versions)
                      (append other-info args)))))))
    (mapc #'analyze-group groups)
    project))

(defun analyze-projects (projects
                         &rest args &key
                         generator-version
                         cache-directory
                         temp-directory)
  (declare (ignore cache-directory temp-directory))
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
        (error "~@<Could not find version ~S in project ~
                ~/print-items:format-print-items/.~@:>"
               version (print-items:print-items project)))))

(defun resolve-project-versions (versions)
  (mapcan (lambda (project-include)
            (let ((project    (project project-include))
                  (version    (jenkins.model.project:version project-include))
                  (parameters (direct-variables project-include)))
              (with-simple-restart
                  (continue "~@<Skip version ~A of project ~A.~@:>"
                            version project)
                (list (make-instance 'resolved-project-include
                                     :specification project-include
                                     :version       (resolve-project-version
                                                     project version)
                                     :variables     parameters)))))
          versions))

;;; Distributions

(defun set-overwrites (distributions overwrites)
  (with-sequence-progress (:overwrites distributions)
    (map (class-of distributions)
         (lambda (distribution)
           (progress "~/print-items:format-print-items/"
                     (print-items:print-items distribution))
           (with-simple-restart
               (continue "~@<Do not overwrite variables in ~S.~@:>"
                         distribution)
             (iter (for (name . value) in overwrites)
                   (log:info "~@<In ~A, setting ~S to ~S.~@:>"
                             distribution name value)
                   (setf (lookup distribution name) value))
             (let ((recipe-maintainers
                    (ensure-persons!
                     (jenkins.analysis::parse-people-list
                      (value/cast distribution :recipe.maintainer '())))))
               (reinitialize-instance
                distribution :persons `(:recipe.maintainer ,recipe-maintainers))
               distribution)))
         distributions)))
