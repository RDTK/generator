;;;; command-list-projects.lisp --- List projects included in a distribution.
;;;;
;;;; Copyright (C) 2017, 2018, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:build-generator.commands)

(defclass list-projects (distribution-input-mixin
                         mode-mixin)
  ()
  (:documentation
   "List projects included in a given distribution."))

(service-provider:register-provider/class
 'command :list-projects :class 'list-projects)

(build-generator.commandline-options:define-option-mapping
    (*command-schema* "list-projects")
  (&rest "distributions" "DISTRIBUTION-NAME" t))

(defmethod command-execute ((command list-projects))
  (let+ (((&accessors-r/o distributions mode overwrites)
          command)
         ((&values &ign projects)
          (generate-load distributions mode overwrites
                         :generator-version (generator-version))))
    (map nil (lambda (project)
               (format t "~A~%" (truename
                                 (text.source-location:name
                                  (text.source-location:source
                                   (project::location-of project))))))
         projects)))
