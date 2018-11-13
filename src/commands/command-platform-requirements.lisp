;;;; command-platform-requirements.lisp --- Compute requirements for a given platform.
;;;;
;;;; Copyright (C) 2017, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.project.commands)

(defclass platform-requirements (distribution-input-mixin
                                 mode-mixin)
  ((platform :initarg  :platform
             :type     string
             :reader   platform
             :documentation
             #.(format nil "The platform for which dependencies should ~
                be computed.~@
                ~@

                The platform is specified as a space-separated ~
                sequence of increasingly specific component ~
                strings:~@
                ~@
                ~2@TSYSTEM-NAME [SYSTEM-VERSION [ARCHITECTURE]]~@
                ~@
                Examples:~@
                ~@
                • \"ubuntu\"~@
                ~@
                • \"ubuntu bionic\"~@
                ~@
                • \"ubuntu bionic x86_64\"")))
  (:documentation
   "Analyze system packages required on a given platform."))

(service-provider:register-provider/class
 'command :platform-requirements :class 'platform-requirements)

(jenkins.project.commandline-options:define-option-mapping
    (*command-schema* "platform-requirements")
  (&rest               "distributions" "DISTRIBUTION-NAME"   t)

  (("--mode" "-m")     "mode"          "MODE")
  (("--set" "-D")      "overwrites"    "VARIABLE-NAME=VALUE")

  (("--platform" "-p") "platform"      "PLATFORM-SPEC"       t))

(defmethod command-execute ((command platform-requirements))
  (let+ (((&accessors-r/o distributions mode overwrites platform) command)
         ((&values distributions projects)
          (generate-load distributions mode overwrites
                         :generator-version (generator-version)))
         (distributions
          (generate-analyze distributions projects
                            :generator-version (generator-version)
                            :cache-directory   *cache-directory*
                            :temp-directory    *temp-directory*))
         (distributions
          (as-phase (:instantiate)
            (mapcan (lambda (distribution-spec)
                      (when-let ((distribution (instantiate distribution-spec)))
                        (list distribution)))
                    distributions)))
         (platform     (split-sequence:split-sequence #\Space platform))
         (requirements (as-phase (:check-platform-requirements)
                         (platform-requires distributions platform))))
    (report-platform-requirements requirements platform)))
