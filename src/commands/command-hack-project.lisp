;;;; command-hack-project.lisp --- Checkout projects into a workspace.
;;;;
;;;; Copyright (C) 2017, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.project.commands)

(defclass hack-project (output-directory-mixin)
  ((projects :initarg  :projects
             :type     (or null (cons recipe-and-version list))
             :reader   projects
             :documentation
             #.(format nil "Filename(s) of project recipe(s) with ~
                version(s) that should be made available for ~
                development.")))
  (:documentation
   "Make project(s) available for development in local workspaces."))

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
