;;;; command-report.lisp --- Generate report for distributions.
;;;;
;;;; Copyright (C) 2017, 2018, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.project.commands)

(defclass report (distribution-input-mixin
                  mode-mixin
                  output-directory-mixin)
  ((kind :initarg :kind
         :type    (or null (cons (member :json :graph :catalog)))
         :reader  kind
         :documentation
         "The kind(s) of report(s) that should be generated."))
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

(jenkins.project.commandline-options:define-option-mapping
    (*command-schema* "report")
  (&rest                       "distributions"    "DISTRIBUTION-NAME"   t)

  (("-m" "--mode")             "mode"             "MODE")
  (("-D" "--set")              "overwrites"       "VARIABLE-NAME=VALUE")

  (("-k" "--kind")             "kind"             "KIND"                t)
  (("-o" "--output-directory") "output-directory" "DIRECTORY"           t))

(defmethod command-execute ((command report))
  (let+ (((&accessors-r/o distributions mode overwrites kind output-directory) command)
         ((&values distributions projects)
          (generate-load distributions mode overwrites
                         :generator-version (generator-version)))
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
                   (jenkins.report:report distributions kind output-directory))))
         kind)))
