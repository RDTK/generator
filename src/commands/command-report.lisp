;;;; command-report.lisp --- Generate report for distributions.
;;;;
;;;; Copyright (C) 2017-2022 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:build-generator.commands)

(defclass report (distribution-input-mixin
                  mode-mixin
                  output-directory-mixin)
  ((kind     :initarg :kind
             :type    (or null (cons (member :json :graph :catalog)))
             :reader  kind
             :documentation
             "The kind(s) of report(s) that should be generated.")
   (platform :initarg :platform
             :type     (or null (cons string))
             :reader   platform
             :initform '()
             :documentation
             #.(format nil "If the specified report kind includes ~
                platform dependencies, the platform(s) for which ~
                dependencies should be computed.~@
                ~@
                A platform is specified as a space-separated sequence ~
                of increasingly specific component strings:~@
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
   #.(format nil "Generate one or more reports for given ~
      distribution(s).~@
      ~@
      More concretely, write information about distributions and ~
      projects into one or more report files. The written information ~
      includes most of the content of the respective underlying recipe ~
      but also expanded variable values, inferred variable values and ~
      analysis results.")))

(service-provider:register-provider/class
 'command :report :class 'report)

(build-generator.commandline-options:define-option-mapping
    (*command-schema* "report")
  (&rest                       "distributions"    "DISTRIBUTION-NAME"   t)

  (("-m" "--mode")             "mode"             "MODE")
  (("-D" "--set")              "overwrites"       "VARIABLE-NAME=VALUE")

  (("-k" "--kind")             "kind"             "KIND"                t)
  (("-o" "--output-directory") "output-directory" "DIRECTORY"           t)
  (("-p" "--platform")         "platform"         "PLATFORM-SPEC"))

(defmethod command-execute ((command report))
  (let+ (((&accessors-r/o distributions mode overwrites kind platform output-directory)
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
    (map nil (lambda (kind)
               (as-phase ((symbolicate '#:report/ kind))
                 (with-simple-restart (continue "Skip ~(~A~) report" kind)
                   (when (eq kind :graph)
                     (unless (setf cl-dot:*dot-path* (cl-dot::find-dot))
                       (error "~@<Could not find dot program.~@:>")))
                   (let ((target (if (eq kind :catalog)
                                     (let ((platforms (mapcar (lambda (platform)
                                                                (split-sequence:split-sequence
                                                                 #\Space platform :remove-empty-subseqs t))
                                                              platform)))
                                       (cons output-directory platforms)) ; HACK
                                     output-directory)))
                     (build-generator.report:report distributions kind target)))))
         kind)))
