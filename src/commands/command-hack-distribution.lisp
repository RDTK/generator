;;;; command-hack-distribution.lisp --- Checkout distributions into a workspace.
;;;;
;;;; Copyright (C) 2017, 2018, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:build-generator.commands)

(defclass hack-distribution (distribution-input-mixin
                             mode-mixin
                             output-directory-mixin)
  ((bare? :initarg  :bare?
          :type     boolean
          :reader   bare?
          :initform nil))
  (:documentation
   "Make distribution(s) available for development in local workspaces."))

(service-provider:register-provider/class
 'command :hack-distribution :class 'hack-distribution)

(build-generator.commandline-options:define-option-mapping
    (*command-schema* "hack-distribution")
  (&rest                       "distributions"    "DISTRIBUTION-RECIPE" t)

  (("--set" "-D")              "overwrites"       "VARIABLE-NAME=VALUE")

  (("--output-directory" "-o") "output-directory" "DIRECTORY"           t)
  (("--bare")                  "bare?"))

(defmethod command-execute ((command hack-distribution))
  (reinitialize-instance project::*global-stuff* :variables (overwrites command))

  (let+ ((generator-version (generator-version))
         ((&accessors-r/o distributions mode overwrites output-directory bare?)
          command)
         ;; Load templates, distributions and projects.
         ((&values distributions projects)
          (generate-load distributions mode overwrites
                         :generator-version generator-version))
         ;; Analyze projects.
         (distributions
          (generate-analyze distributions projects
                            :generator-version generator-version
                            :cache-directory   *cache-directory*
                            :temp-directory    *temp-directory*)))
    ;; Check projects into output directory.
    (as-phase (:retrieve/project)
      (build-generator.analysis::with-git-cache ()
        (let ((git-cache       build-generator.analysis::*git-cache*)
              (cache-directory *cache-directory*))
          (with-sequence-progress (:retrieve/project projects)
            (lparallel:pmapc
             (lambda (project)
               (progress "~/print-items:format-print-items/"
                         (print-items:print-items project))
               (more-conditions::without-progress
                 (let ((build-generator.analysis::*git-cache*     git-cache)
                       (build-generator.analysis::*cache-version* generator-version))
                   (with-simple-restart
                       (continue "~@<Skip project ~A.~@:>" project)
                     (access-project project output-directory
                                     :cache-directory cache-directory
                                     :bare?           bare?)))))
             :parts most-positive-fixnum projects)))))
    ;; Report unresolved platform requirements.
    (as-phase (:check-platform-requirements)
      (let+ (((&values unresolved platform)
              (unresolved-platform-requirements distributions)))
        (cond (unresolved
               (report-platform-requirements
                unresolved platform :label "unresolved"))
              ((not platform)
               (warn "~@<Platform not known - could not check platform ~
                      requirements.~@:>~%")))))))

;;; Utilities

(defun output-directory-for-project (output-directory name &optional version)
  (let ((directory `(:relative ,@(ensure-list name)
                               ,@(when version (ensure-list version)))))
    (merge-pathnames (make-pathname :directory directory) output-directory)))

(defun access-project (project output-directory &key cache-directory bare?)
  (log:debug "~@<Retrieving ~A into ~S~@:>"
             project output-directory)
  (let ((groups (group-project-versions-for-analysis project)))
    (mapc (lambda+ ((info . versions))
            (when-let ((repository (getf info :repository)))
              (let ((other-info (remove-from-plist info :repository)))
                (if bare?
                    (access-project-repository
                     project repository other-info output-directory
                     :cache-directory cache-directory)
                    (mapc (rcurry #'access-project-version
                                  repository other-info output-directory
                                  :cache-directory cache-directory)
                          versions)))))
          groups)))

(defun access-project-repository (project repository info output-directory
                                  &key cache-directory)
  (let* ((project-name (split-sequence:split-sequence
                        #\/ (multiple-value-call #'project::apply-replacements
                              :output-replacements
                              (var:value project :repository
                                         (model:name project)))))
         (directory    (output-directory-for-project
                        output-directory project-name)))
    (apply #'access-source (puri:uri repository) :auto directory
           :cache-directory cache-directory
           :bare?           t
           info)))

(defun access-project-version (version repository info output-directory
                               &key cache-directory)
  (let* ((project-name (model:name (model:parent version)))
         (version-name (model:name version))
         (info*         (resolve-analysis-variables version)) ; TODO do we need both infos?
         (directory    (output-directory-for-project
                        output-directory project-name version-name)))
    (apply #'access-source (puri:uri repository) :auto directory
           :cache-directory cache-directory
           (append info info*))))

;;; Stub access implementation

(defgeneric access-source (source kind target &key &allow-other-keys))

(defmethod access-source ((source puri:uri)
                          (kind   (eql :auto))
                          (target t)
                          &rest args &key scm)
  (let ((scm (build-generator.analysis::guess-scm source scm)))
    (apply #'access-source source scm target
           (remove-from-plist args :scm))))

(defmethod access-source ((source puri:uri)
                          (kind   (eql :git))
                          (target pathname)
                          &rest args &key
                          cache-directory bare?)
  (if bare?
      (build-generator.analysis::clone-git-repository/maybe-cached
       source target :cache-directory cache-directory :bare? t)
      (let+ (((&flet find-commitish (version)
                (or (when-let ((commit (getf version :commit)))
                      (list :commit commit))
                    (when-let ((branch (or (getf version :tag)
                                           (getf version :branch))))
                      (list :branch branch))
                    (error "~@<No commit, tag or branch specified in ~
                            ~S~@:>"
                           version)))))
        ;; TODO sub-directory
        (apply #'build-generator.analysis::clone-git-repository/maybe-cached
               source target :cache-directory cache-directory
               (find-commitish (remove-from-plist args :cache-directory))))))

(defmethod access-source ((source puri:uri)
                          (kind   (eql :svn))
                          (target pathname)
                          &key
                          )
  (build-generator.analysis::checkout-subversion-repository
   source target))

(defmethod access-source ((source puri:uri)
                          (kind   (eql :mercurial))
                          (target pathname)
                          &key))

(defmethod access-source ((source puri:uri)
                          (kind   (eql :archive))
                          (target pathname)
                          &key))
