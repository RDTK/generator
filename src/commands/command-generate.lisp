;;;; command-generate.lisp --- Generate Jenkins jobs for a distribution.
;;;;
;;;; Copyright (C) 2017, 2018, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:build-generator.commands)

(defclass generate (distribution-input-mixin
                    mode-mixin
                    jenkins-access-mixin)
  ((delete-other?        :initarg  :delete-other?
                         :type     boolean
                         :reader   delete-other?
                         :initform nil
                         :documentation
                         #.(format nil "Delete previously ~
                            automatically generated jobs when they ~
                            are not re-created in this generation ~
                            run."))
   (delete-other-pattern :initarg  :delete-other-pattern
                         :type     (or null string)
                         :reader   delete-other-pattern
                         :initform nil
                         :documentation
                         #.(format nil "When deleting previously ~
                            automatically generated jobs, only ~
                            consider jobs whose name matches the ~
                            regular expression REGEX.~@
                            ~@
                            The default value corresponds to the ~
                            common case of deleting only jobs ~
                            belonging to previous versions of the ~
                            distribution(s) being generated, i.e. the ~
                            regular expression ~
                            (DISTRIBUTION-NAME₁|DISTRIBUTION-NAME₂|…)$.")))
  (:documentation
   "Generate Jenkins jobs for a given distribution."))

(service-provider:register-provider/class
 'command :generate :class 'generate)

(build-generator.commandline-options:define-option-mapping
    (*command-schema* "generate")
  (&rest                    "distributions"        "DISTRIBUTION-NAME"   t)

  (("--mode" "-m")          "mode"                 "MODE")
  (("--set" "-D")           "overwrites"           "VARIABLE-NAME=VALUE")

  ("--delete-other"         "delete-other?")
  ("--delete-other-pattern" "delete-other-pattern" "REGEX")

  (("--base-uri" "-b")      "base-uri"             "URI")
  (("--username" "-u")      "username"             "LOGIN")
  (("--password" "-p")      "password"             "PASSWORD")
  (("--api-token" "-t" "-a") "api-token"            "API-TOKEN"))

(defmethod command-execute ((command generate))
  (let+ (((&accessors-r/o distributions mode overwrites
                          delete-other? delete-other-pattern)
          command)
         ((&values distributions projects)
          (generate-load distributions mode overwrites
                         :generator-version (generator-version)
                         :cache-directory   *cache-directory*))
         (distributions
          (generate-analyze distributions projects
                            :generator-version (generator-version)
                            :temp-directory    *temp-directory*
                            :cache-directory   *cache-directory*
                            :age-limit         *age-limit*))
         (distributions
          (as-phase (:instantiate)
            (mapcan (lambda (distribution-spec)
                      (when-let ((distribution (model:instantiate distribution-spec)))
                        (list distribution)))
                    distributions))))
    (as-phase (:check-access ; :continuable? nil
               )
      (check-distribution-access distributions))
    (generate-deploy distributions
                     :delete-other?        delete-other?
                     :delete-other-pattern delete-other-pattern)))

;;; Functions

(defun generate-load (distributions mode overwrites
                      &key generator-version
                           cache-directory)
  (let+ (((&values repository distributions)
          (let ((repository (derive-root-repository (first distributions) mode
                                                    :cache-directory cache-directory)))
            ;; Transform distributions pathnames into a suitable form.
            (values repository
                    (let ((distributions-directory (merge-pathnames
                                                    "distributions/"
                                                    (project:root-directory repository))))
                      (map 'list (lambda (distribution)
                                   (uiop:enough-pathname (merge-pathnames distribution)
                                                         distributions-directory))
                           distributions)))))
         ((&flet locate-and-load (kind pattern loader
                                  &key (if-no-match nil if-no-match-supplied?))
            (let* ((files   (as-phase ((symbolicate :locate/ kind))
                              (apply #'locate-specifications kind pattern repository
                                     (when if-no-match-supplied?
                                       (list :if-no-match if-no-match)))))
                   (objects (as-phase ((symbolicate :load/ kind))
                              (load-specifications kind files loader repository))))
              (values objects files))))
         ;; Templates
         (template-patterns       (project:recipe-truenames repository :template :wild))
         (templates               (locate-and-load
                                   :template template-patterns
                                   (rcurry #'project:load-template/yaml
                                           :generator-version generator-version)))
         ;; Persons
         (person-patterns         (project:recipe-truenames repository :person :wild))
         (persons                 (locate-and-load
                                   :person person-patterns
                                   (rcurry #'project:load-person/yaml
                                           :generator-version generator-version)
                                   :if-no-match '()))
         ;; Distributions
         (distributions           (locate-and-load
                                   :distribution distributions
                                   (rcurry #'project:load-distribution/yaml
                                           :generator-version generator-version
                                           :overwrites        overwrites)))
         (distributions           (as-phase (:parse-persons)
                                    (parse-distribution-persons distributions)))
         ;; Projects
         (projects-files+versions (as-phase (:locate/project)
                                    (locate-projects distributions repository)))
         (projects                (as-phase (:load/project)
                                    (load-projects/versioned
                                     projects-files+versions repository
                                     :generator-version generator-version))))
    (values distributions projects persons)))

(defun generate-analyze (distributions projects
                         &key
                         generator-version
                         temp-directory
                         cache-directory
                         age-limit)
  (let+ ((analyzed-projects (as-phase (:analyze)
                              (analyze-projects
                               projects
                               :generator-version generator-version
                               :temp-directory    temp-directory
                               :cache-directory   cache-directory
                               :age-limit         age-limit)))
         (seen (make-hash-table :test #'eq))
         ((&labels resolve-versions (distribution)
            (ensure-gethash
             distribution seen
             (let ((includes (project:direct-includes distribution))
                   (versions (project:direct-versions distribution)))
               (map nil (compose #'resolve-versions #'project:distribution) includes)
               (reinitialize-instance
                distribution :direct-versions (resolve-project-versions versions))))))
         (distributions (as-phase (:resolve/distribution)
                          (map 'list #'resolve-versions distributions))))
    (values distributions analyzed-projects)))

(defun generate-deploy (distributions
                        &key
                        delete-other?
                        delete-other-pattern)
  (let+ ((projects (mappend #'project:versions distributions))
         (jobs/specs (as-phase (:deploy/project)
                       (let ((jobs (deploy-projects projects)))
                         (when (some (lambda (job)
                                       (not (eq (var:value/cast job :dependencies.mode) :none)))
                                     jobs)
                           (deploy-job-dependencies jobs))
                         jobs)))
         (jobs       (mappend #'model:implementations jobs/specs))
         ((&values &ign orchestration-jobs)
          (as-phase (:orchestration)
            (configure-distributions distributions)))
         (all-jobs   (append jobs (mappend #'model:implementations
                                           orchestration-jobs))))
    (when delete-other?
      (delete-other-jobs all-jobs (make-delete-other-pattern
                                   delete-other-pattern distributions)))

    (as-phase (:list-credentials)
      (list-credentials jobs))))

(defun generated? (job)
  (search "automatically generated" (jenkins.api:description job)))

(defun make-delete-other-pattern (pattern distributions)
  (cond (pattern)
        ((length= 1 distributions)
         `(:sequence ,(model:name (first distributions)) :end-anchor))
        (t
         `(:sequence
           (:alternation ,@(map 'list #'model:name distributions))
           :end-anchor))))

(defun delete-other-jobs (all-jobs pattern)
  (as-phase (:delete-other-jobs)
    (log:info "Deleting other jobs using pattern ~S" pattern)
    (let* ((other-jobs     (set-difference
                            (jenkins.api:all-jobs pattern) all-jobs
                            :key #'jenkins.api:id :test #'string=))
           (generated-jobs (remove-if-not #'generated? other-jobs)))
      (with-sequence-progress (:delete-other generated-jobs)
        (mapc (progressing #'jenkins.api:delete-job :delete-other)
              generated-jobs)))))
