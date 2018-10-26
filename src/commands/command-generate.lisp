;;;; command-generate.lisp --- Generate Jenkins jobs for a distribution.
;;;;
;;;; Copyright (C) 2017, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.project.commands)

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
                         :type     string
                         :reader   delete-other-pattern
                         :initform ".*"
                         :documentation
                         #.(format nil "When deleting previously ~
                            automatically generated jobs, only ~
                            consider jobs whose name matches the ~
                            regular expression REGEX.~@
                            ~@
                            A common case, deleting only jobs ~
                            belonging to the distribution being ~
                            generated, can be achieved using the ~
                            regular expression ~
                            DISTRIBUTION-NAME$.")))
  (:documentation
   "Generate Jenkins jobs for a given distribution."))

(service-provider:register-provider/class
 'command :generate :class 'generate)

(jenkins.project.commandline-options:define-option-mapping
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
                         :generator-version (generator-version)))
         ((&values distributions analyzed-projects)
          (generate-analyze distributions projects
                            :generator-version (generator-version)
                            :cache-directory   *cache-directory*
                            :temp-directory    *temp-directory*)))
    (generate-check distributions)
    (let ((projects (as-phase (:instantiate/project)
                      (instantiate-projects analyzed-projects distributions))))
      (generate-deploy distributions projects
                       :delete-other?        delete-other?
                       :delete-other-pattern delete-other-pattern))))

;;; Functions

(defun generate-load (distributions mode overwrites
                      &key generator-version)
  (let+ (((&flet locate-and-load (kind pattern loader
                                  &key (if-no-match nil if-no-match-supplied?))
            (let* ((files   (as-phase ((symbolicate :locate/ kind))
                              (apply #'locate-specifications kind pattern
                                     (when if-no-match-supplied?
                                       (list :if-no-match if-no-match)))))
                   (objects (as-phase ((symbolicate :load/ kind))
                              (load-specifications kind files loader))))
              (values objects files))))
         ;; Templates
         (template-patterns       (list (derive-template-pattern
                                         (first distributions) mode)))
         (templates
          (locate-and-load :template template-patterns
                           (rcurry #'load-template/yaml
                                   :generator-version generator-version)))
         ;; Persons
         (person-patterns         (list (merge-pathnames
                                         (make-pathname
                                          :name      :wild
                                          :type      "person"
                                          :directory `(:relative :back "persons"))
                                         (first distributions))))
         (persons
          (locate-and-load :person person-patterns
                           (rcurry #'load-person/yaml
                                   :generator-version generator-version)
                           :if-no-match '()))
         ;; Distributions
         ((&values distributions distribution-files)
          (locate-and-load :distribution distributions
                           (rcurry #'load-distribution/yaml
                                   :generator-version generator-version)))
         (distributions           (as-phase (:overwrites)
                                    (set-overwrites distributions overwrites)))
         ;; Projects
         (projects-files+versions (as-phase (:locate/project)
                                    (locate-projects distribution-files distributions)))
         (projects                (as-phase (:load/project)
                                    (load-projects/versioned
                                     projects-files+versions
                                     :generator-version generator-version))))
    (values distributions projects persons)))

(defun generate-analyze (distributions projects
                         &key
                         generator-version
                         cache-directory
                         temp-directory)
  (let ((analyzed-projects (as-phase (:analyze)
                             (analyze-projects
                              projects
                              :generator-version generator-version
                              :cache-directory   cache-directory
                              :temp-directory    temp-directory)))
        (distributions     (as-phase (:resolve/distribution)
                             (mapcar (lambda (distribution)
                                       (reinitialize-instance
                                        distribution
                                        :versions (resolve-project-versions
                                                   (jenkins.model.project:versions distribution))))
                                     distributions))))
    (values distributions analyzed-projects)))

(defun generate-check (distributions)
  (as-phase (:check-access ; :continuable? nil
             )
    (check-distribution-access distributions)))

(defun generated? (job)
  (search "automatically generated" (jenkins.api:description job)))

(defun generate-deploy (distributions projects
                        &key
                        delete-other?
                        delete-other-pattern)
  (let+ ((jobs/specs (as-phase (:deploy/project)
                       (let ((jobs (deploy-projects projects)))
                         (when (some (lambda (job)
                                       (not (eq (value/cast job :dependencies.mode) :none)))
                                     jobs)
                           (deploy-job-dependencies jobs))
                         jobs)))
         (jobs       (mappend #'implementations jobs/specs))
         ((&values &ign orchestration-jobs)
          (as-phase (:orchestration)
            (configure-distributions distributions)))
         (all-jobs   (append jobs (mappend #'implementations
                                           orchestration-jobs))))
    (when delete-other?
      (as-phase (:delete-other-jobs)
        (let* ((other-jobs     (set-difference
                                (jenkins.api:all-jobs delete-other-pattern)
                                all-jobs
                                :key #'jenkins.api:id :test #'string=))
               (generated-jobs (remove-if-not #'generated? other-jobs)))
          (with-sequence-progress (:delete-other generated-jobs)
            (mapc (progressing #'jenkins.api:delete-job :delete-other)
                  generated-jobs)))))

    (as-phase (:list-credentials)
      (list-credentials jobs))))
