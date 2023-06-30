;;;; command-hack-distribution.lisp --- Checkout distributions into a workspace.
;;;;
;;;; Copyright (C) 2017-2023 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:build-generator.commands)

(defclass hack-distribution (distribution-input-mixin
                             mode-mixin
                             output-directory-mixin)
  ((bare? :initarg  :bare?
          :type     boolean
          :reader   bare?
          :initform nil
          :documentation
          "Create \"bare\" repositories when supported."))
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
      (if bare?
          (retrieve-bare-repositories projects output-directory
                                      :cache-directory   *cache-directory*
                                      :generator-version generator-version)
          (retrieve-projects projects output-directory
                             :cache-directory   *cache-directory*
                             :generator-version generator-version)))
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

(defun pmap-with-caches (function sequence &key generator-version)
  (build-generator.analysis::with-git-cache ()
    (let ((git-cache build-generator.analysis::*git-cache*))
      (with-sequence-progress (:retrieve/project sequence)
        (lparallel:pmapc
         (lambda (item)
           (progress "~/print-items:format-print-items/"
                     (print-items:print-items item))
           (more-conditions::without-progress
             (let ((build-generator.analysis::*git-cache*     git-cache)
                   (build-generator.analysis::*cache-version* generator-version))
               (with-simple-restart
                   (continue "~@<Skip ~A.~@:>" item)
                 (funcall function item)))))
         :parts most-positive-fixnum sequence)))))

(defun output-directory-for-project (output-directory name &optional version)
  (let ((directory `(:relative ,@(ensure-list name)
                               ,@(when version (ensure-list version)))))
    (merge-pathnames (make-pathname :directory directory) output-directory)))

;;; Retrieving bare repositories

(defclass repository-access (print-items:print-items-mixin)
  ((%repository :initarg  :repository
                :reader   repository)
   (%versions   :accessor versions
                :initform nil)
   (%other-info :initarg  :other-info
                :reader   other-info)))

(defmethod print-items:print-items append ((object repository-access))
  `((:repository                      "~A"             ,(repository object))
    ((:versions (:after :repository)) " ~D version~:P" ,(length (versions object)))))

(defun retrieve-bare-repositories (projects output-directory
                                   &key cache-directory generator-version)
  (let ((repositories (make-hash-table :test #'equal)))
    (mapc (lambda (project)
            (let ((groups (group-project-versions-for-analysis project)))
              (mapc (lambda+ ((info . versions))
                      (when-let ((repository (getf info :repository)))
                        (let ((access (ensure-gethash
                                       repository repositories
                                       (let ((other-info (remove-from-plist info :repository)))
                                         (make-instance 'repository-access
                                                        :repository repository
                                                        :other-info other-info)))))
                          (unionf (versions access) versions))))
                    groups)))
          projects)
    (pmap-with-caches
     (lambda (access)
       (let ((repository (repository access))
             (versions   (versions access))
             (other-info (other-info access)))
         (access-project-repository
          repository versions other-info output-directory
          :cache-directory cache-directory)))
     (hash-table-values repositories) :generator-version generator-version)))

(defun access-project-repository (repository versions info output-directory
                                  &key cache-directory)
  (let* ((project-name (split-sequence:split-sequence
                        #\/ (project::apply-replacements
                             :output-replacements repository nil)))
         (directory    (output-directory-for-project
                        output-directory project-name)))
    (apply #'access-source (puri:uri repository) :auto directory
           :versions        versions
           :cache-directory cache-directory
           :bare?           t
           info)))

;;; Retrieving projects

(defun retrieve-projects (projects output-directory &key cache-directory
                                                         generator-version)
  (pmap-with-caches (lambda (project)
                      (access-project project output-directory
                                      :cache-directory cache-directory))
                    projects :generator-version generator-version))

(defun access-project (project output-directory &key cache-directory)
  (log:debug "~@<Retrieving ~A into ~S~@:>" project output-directory)
  (let ((groups (group-project-versions-for-analysis project)))
    (mapc (lambda+ ((info . versions))
            (when-let ((repository (getf info :repository)))
              (let ((other-info (remove-from-plist info :repository)))
                (mapc (rcurry #'access-project-version
                              repository other-info output-directory
                              :cache-directory cache-directory)
                      versions))))
          groups)))

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

(defgeneric access-source (source kind target &key &allow-other-keys)
  (:method ((source puri:uri) (kind t) (target pathname) &key)
    (error "~@<Making ~A repositories available locally is not ~
            implemented.~@:>"
           kind)))

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
               (find-commitish (remove-from-plist args :cache-directory)))))

  (build-generator.analysis::%run-git
   `("remote" "set-url" "origin" ,(princ-to-string source)) target))

(defmethod access-source ((source puri:uri)
                          (kind   (eql :svn))
                          (target pathname)
                          &key)
  (build-generator.analysis::checkout-subversion-repository
   source target))

(defmethod access-source ((source puri:uri)
                          (kind   (eql :archive))
                          (target pathname)
                          &key)
  (let* ((archive-name (lastcar (puri:uri-parsed-path source)))
         (output-file  (merge-pathnames archive-name target)))
    (ensure-directories-exist output-file)
    (build-generator.analysis::download-file source output-file)))
