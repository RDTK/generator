;;;; command-generate.lisp --- Generate Jenkins jobs for a distribution.
;;;;
;;;; Copyright (C) 2017-2022 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:build-generator.commands)

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
         (template-patterns       (project:recipe-truenames repository :template :wild-inferiors))
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
