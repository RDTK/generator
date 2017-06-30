;;;; command-hack.lisp --- Checkout projects into a workspace.
;;;;
;;;; Copyright (C) 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.project.commands)

;;; `hack-project'

(defclass hack-project (output-directory-mixin)
  ((projects :initarg  :projects
             :type     (or null (cons recipe-and-version list))
             :reader   projects
             :documentation
             #.(format nil "Filenames of project recipes with versions ~
                that should be made available for development.")))
  (:documentation
   "Make projects available for development in local workspaces."))

(service-provider:register-provider/class
 'command :hack-project :class 'hack-project)

(jenkins.project.commandline-options:define-option-mapping
    (*command-schema* "hack-project")
  (&rest                       "projects"         "PROJECT-RECIPE" t)

  (("--output-directory" "-o") "output-directory" "DIRECTORY"      t))

(defmethod command-execute ((command hack-project))
  (let+ (((&accessors-r/o projects output-directory) command))
    (map nil (lambda+ ((project . version))
               (log:warn "~@<Retrieving ~A@~A into ~S~@:>"
                         project version output-directory))
         projects)))

;;; `hack-distribution'

(defclass hack-distribution (distribution-input-mixin
                             mode-mixin
                             output-directory-mixin)
  ()
  (:documentation
   "Make distributions available for development in local workspaces."))

(service-provider:register-provider/class
 'command :hack-distribution :class 'hack-distribution)

(jenkins.project.commandline-options:define-option-mapping
    (*command-schema* "hack-distribution")
  (&rest                       "distributions"    "DISTRIBUTION-RECIPE" t)

  (("--output-directory" "-o") "output-directory" "DIRECTORY"           t))

(defmethod command-execute ((command hack-distribution))
  (let+ (((&accessors-r/o distributions mode overwrites output-directory)
          command)
         ((&values &ign projects)
          (generate-load distributions mode overwrites
                         :generator-version (generator-version)))
         ((&flet hack-on-project (project)
            (log:warn "~@<Retrieving ~A into ~S~@:>"
                      project output-directory)
            (let ((groups (group-project-versions-for-analysis project)))
              (log:warn groups)))))
    (as-phase (:retrieve/project)
      (with-sequence-progress (:retrieve/project projects)
        (map nil (lambda (project) ; TODO pmapc
                   (progress "~A" project)
                   (hack-on-project project))
            projects)))))
