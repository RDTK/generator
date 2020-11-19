;;;; aspects-publish.lisp --- Definitions of publisher-creating aspects
;;;;
;;;; Copyright (C) 2012-2020 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:build-generator.model.aspects)

;;; Tasks aspect

(define-aspect (tasks :plugins ("warnings-ng"))
    (publisher-defining-mixin)
    ((pattern               :type (var:list-of string)
      :documentation
      "Filename patterns specifying which workspace files to scan for
       open tasks.")
     ((exclude         '()) :type (var:list-of string)
      :documentation
      "Filename patterns specifying which workspace files to exclude
       from the scan for open tasks.")
     ((keywords.low    '()) :type (var:list-of string)
      :documentation
      "Keywords indicating low-priority open tasks.")
     ((keywords.normal '()) :type (var:list-of string)
      :documentation
      "Keywords indicating normal-priority open tasks.")
     ((keywords.high   '()) :type (var:list-of string)
      :documentation
      "Keywords indicating high-priority open tasks.")
     ((implementation  :ng) :type (or (eql :legacy) (eql :ng))
      :documentation
      "Which of Jenkins' architectures for scanning and reporting
       warnings should be used?"))
  "Adds an open tasks publisher to the generated job."
  (case implementation
    (:ng
     (with-interface (jenkins.api:publishers job)
         (issues-recorder (jenkins.api:publisher/issues-recorder))
       (removef (jenkins.api:analysis-tools issues-recorder)
                'jenkins.api:analysis-tool/open-tasks
                :key #'type-of)
       (push (make-instance 'jenkins.api:analysis-tool/open-tasks
                            :id                  "open-tasks"
                            :name                "Open Tasks"
                            :include-pattern     pattern
                            :exclude-pattern     exclude
                            :high-tags           keywords.high
                            :normal-tags         keywords.normal
                            :low-tags            keywords.low
                            :ignore-case?        t
                            :regular-expression? nil)
             (jenkins.api:analysis-tools issues-recorder))))
    (:legacy
     (push (constraint! (publish)
             (make-instance 'jenkins.api:publisher/tasks
                            :pattern         pattern
                            :exclude         exclude
                            :keywords/low    keywords.low
                            :keywords/normal keywords.normal
                            :keywords/high   keywords.high))
           (jenkins.api:publishers job)))))

;;; SLOCcount aspect

(define-aspect (sloccount :plugins ("sloccount"))
    (builder-defining-mixin publisher-defining-mixin)
    ((directories :type list))
  "Adds a sloccount publisher to the generated job."
  (let ((command (format nil "DATA_DIR=$(mktemp -d /tmp/build-generator.sloccount.data.XXXXXXXXXX)~@
                              REPORT_DIR=$(mktemp -d /tmp/build-generator.sloccount.report.XXXXXXXXXX)~@
                              mkdir -p \"${REPORT_DIR}\"~@
                              sloccount --datadir \"${DATA_DIR}\" --wide --details ~{\"~A\"~^ ~} ~
                                > \"${REPORT_DIR}/sloccount.sc\"~@
                              mv \"${REPORT_DIR}/sloccount.sc\" \"${WORKSPACE}/sloccount.sc\"~@
                              rm -rf \"${DATA_DIR}\" \"${REPORT_DIR}\""
                         directories)))
    (push (constraint! (build ((:before cmake/unix)
                               (:before maven)
                               (:before setuptools)))
            (make-instance 'jenkins.api:builder/shell :command command))
          (jenkins.api:builders job)))

  (push (constraint! (publish)
          (make-instance 'jenkins.api::publisher/sloccount :pattern "sloccount.sc"))
        (jenkins.api:publishers job)))

;;; Warnings aspect

(defun install-parser/native (issues-recorder tool-class id
                              &key (name    nil)
                                   (pattern nil))
  (remove tool-class (jenkins.api:analysis-tools issues-recorder)
          :key #'type-of)
  (push (make-instance tool-class :id      id
                                  :name    name
                                  :pattern pattern)
        (jenkins.api:analysis-tools issues-recorder)))

(defun install-parser/groovy (issues-recorder parser)
  (when-let ((tools (find-if (lambda (tool)
                               (and (typep tool 'jenkins.api:analysis-tool/groovy)
                                    (equal (jenkins.api:parser tool) parser)))
                             (jenkins.api:analysis-tools issues-recorder))))
    (removef (jenkins.api:analysis-tools issues-recorder) parser))
  (push (make-instance 'jenkins.api:analysis-tool/groovy
                       :id      (format nil "groovy-~(~A~)" parser)
                       :name    (format nil "~@(~{~A~^ ~}~)"
                                        (split-sequence #\- parser))
                       :pattern (list (format nil "~(~A~)-output.log" parser))
                       :parser  (string-downcase parser))
        (jenkins.api:analysis-tools issues-recorder)))

(define-aspect (warnings :job-var job
                         :plugins ("warnings-ng"))
    (publisher-defining-mixin)
    ((parsers              :type (var:list-of string)
      :documentation
      "Names of parsers to apply to the output of the generated job.

       Parsers can be either builtin or defined in the global Jenkins
       configuration.")
     ((implementation :ng) :type (or (eql :legacy) (eql :ng))
      :documentation
      "Which of Jenkins' architectures for scanning and reporting
       warnings should be used?"))
  "Configures a warnings publisher for the generated job."
  (cond ((not parsers))
        ((eq implementation :ng)
         (with-interface (jenkins.api:publishers job)
             (issues-recorder (jenkins.api:publisher/issues-recorder))
           (let+ (((&flet match-parser (parser clause)
                     (let ((scanner (ppcre:create-scanner clause :case-insensitive-mode t)))
                       (ppcre:scan scanner parser))))
                  ((&flet install-parser (parser)
                     (switch (parser :test #'match-parser)
                       ;; Builtin
                       ("^gnu (?:c )?compiler 4 \\(gcc\\)$"
                        (install-parser/native
                         issues-recorder 'jenkins.api::analysis-tool/gcc4
                         "gcc4"))
                       ("^apple llvm compiler \\(clang\\)$")
                       ("^cmake$"
                        issues-recorder 'jenkins.api::analysis-tool/cmake "cmake")
                       ("^maven$"
                        (install-parser/native
                         issues-recorder 'jenkins.api::analysis-tool/maven "maven"))
                       ("^java compiler \\(javac\\)$"
                        (install-parser/native
                         issues-recorder 'jenkins.api::analysis-tool/java "java"))
                       ("^doxygen$")
                       ("^sphinx-build$")
                       ;; Groovy-based
                       ("^build generator$"
                        (install-parser/groovy issues-recorder "build-generator"))
                       ("^build generator dependencies$"
                        (install-parser/groovy
                         issues-recorder "build-generator-dependencies"))
                       (t
                        (install-parser/groovy issues-recorder parser))))))
             (map nil #'install-parser parsers))))

        ((eq implementation :legacy)
         (with-interface (jenkins.api:publishers job)
             (warnings (jenkins.api:publisher/warnings))
           (iter (for parser in parsers)
                 (pushnew (make-instance 'jenkins.api:warning-parser/console :name parser)
                          (jenkins.api:console-parsers warnings)
                          :test #'string=
                          :key  #'jenkins.api:name))))))

;;; Checkstyle and PMD aspects

(macrolet
    ((define (name (tool-name      id)
                   (publisher-name display-name)
                   plugins)
       `(define-aspect (,name :job-var job
                              :plugins ,plugins)
            (publisher-defining-mixin)
            ((pattern              :type list
              :documentation
              "Analysis results should be read from files matching the
               pattern.")
             ((implementation :ng) :type (or (eql :legacy) (eql :ng))
              :documentation
              "Which of Jenkins' architectures for scanning and
               reporting warnings should be used?"))
          ,(format nil "Configures a ~A publisher for the generated job."
                   display-name)
          (case implementation
            (:ng
             (with-interface (jenkins.api:publishers job)
                 (issues-recorder (jenkins.api:publisher/issues-recorder))
               (removef (jenkins.api:analysis-tools issues-recorder) ',tool-name
                        :key #'type-of)
               (push (make-instance ',tool-name
                                    :id      ,id
                                    :name    ,display-name
                                    :pattern pattern)
                     (jenkins.api:analysis-tools issues-recorder))))
            (:legacy
             (removef (jenkins.api:publishers job) ',publisher-name
                      :key #'type-of)
             (when pattern
               (push (constraint! (publish)
                       (make-instance ',publisher-name :pattern pattern))
                     (jenkins.api:publishers job))))))))
  (define checkstyle (jenkins.api:analysis-tool/checkstyle "checkstyle")
                     (jenkins.api:publisher/checkstyle     "CheckStyle")
                     ("warnings-ng"))
  (define pmd        (jenkins.api:analysis-tool/pmd        "pmd")
                     (jenkins.api:publisher/pmd            "PMD")
                     ("warnings-ng")))

;;; Test result aspects

(define-aspect (xunit :job-var job
                      :plugins ("xunit"))
    (publisher-defining-mixin)
    ((kind                      :type string)
     (pattern                   :type (or null string))
     (skip-if-no-test-files?    :type boolean)
     (fail-if-not-new?          :type boolean)
     (delete-output-files?      :type boolean)
     (stop-processing-if-error? :type boolean))
  "Configures a publisher for XUnit test results for the generated job."
  (with-interface (jenkins.api:publishers job)
      (publisher (jenkins.api:publisher/xunit))
    (removef (jenkins.api:types publisher) kind
             :test #'string= :key #'jenkins.api:kind)
    (when pattern
      (push (make-instance 'jenkins.api:xunit/type
                           :kind                      kind
                           :pattern                   pattern
                           :skip-if-no-test-files?    skip-if-no-test-files?
                           :fail-if-not-new?          fail-if-not-new?
                           :delete-output-files?      delete-output-files?
                           :stop-processing-if-error? stop-processing-if-error?)
            (jenkins.api:types publisher)))))

(define-aspect (junit :job-var job
                      :plugins ("junit"))
    (publisher-defining-mixin)
    ((pattern              :type (or null string)
      :documentation
      "Test results should be read from files matching the pattern.")
     (keep-long-stdio?     :type boolean
      :documentation
      "See plugin documentation.")
     (health-scale-factor  :type (or null positive-real)
      :documentation
      "Factor relating test failure counts to job health percentages.

       See plugin documentation for details.")
     (allow-empty-results? :type boolean
      :documentation
      "Controls whether empty test results should result in a build
       failure."))
  "Configures a publisher for JUnit test results for the generated job."
  ;; Remove previous configuration, if any.
  (removef (jenkins.api:publishers job) 'jenkins.api:publisher/junit
           :key #'type-of)
  ;; Add new configuration.
  (when pattern
    (push (constraint! (publish)
            (make-instance 'jenkins.api:publisher/junit
                           :pattern              pattern
                           :keep-long-stdio?     keep-long-stdio?
                           :health-scale-factor  health-scale-factor
                           :allow-empty-results? allow-empty-results?))
          (jenkins.api:publishers job))))

;;; Email notification

(define-aspect (email-notification :job-var job
                                   :plugins ("mailer"))
    (publisher-defining-mixin)
    ((recipients           :type (var:list-of string)
      :documentation
      "A list of email addresses to which notifications in case of a
       build failure should be sent.")
     (send-to-perpetrator? :type boolean
      :documentation
      "Controls whether the authors of build-breaking commits are
       treated as additional recipients."))
  "Adds email notification in case of failed builds to a generated job."
  (removef (jenkins.api:publishers job) 'jenkins.api:publisher/email-notification
           :key #'type-of)
  (when recipients
    (push (constraint! (publish)
            (make-instance 'jenkins.api:publisher/email-notification
                           :recipients           recipients
                           :send-to-individuals? send-to-perpetrator?))
          (jenkins.api:publishers job))))

;;; Upload aspect

(define-aspect (upload :job-var job
                       :plugins ("publish-over-ssh"))
    (publisher-defining-mixin)
    ((target             :type string
      :documentation
      "The name of the machine to which artifacts should be uploaded.")
     (source-files       :type (var:list-of string)
      :documentation
      "List of files to upload.")
     ((excludes     '()) :type (var:list-of string)
      :documentation
      "Patterns for files which should be excluded from the upload.")
     (remove-prefix      :type string
      :documentation
      "A prefix string that should be removed from source filenames to
       obtain target filenames.")
     (remote-directory   :type string
      :documentation
      "Directory on the target machine into which artifacts should be
       uploaded.")
     (verbose?           :type boolean
      :documentation
      "Control the verbosity of the plugin during the upload process."))
  "Adds a publisher for uploads build results to the generated job."
  (push (constraint! (publish)
          (make-instance 'jenkins.api:publisher/ssh
                         :target           target
                         :source-files     source-files
                         :excludes         excludes
                         :remove-prefix    remove-prefix
                         :remote-directory remote-directory
                         :verbose?         verbose?))
        (jenkins.api:publishers job)))

;;; HTML report aspect

(define-aspect (html-report :job-var job
                            :plugins ("htmlpublisher"))
    (publisher-defining-mixin)
    ((name           :type string
      :documentation
      "Name of the report.")
     (base-directory :type string
      :documentation
      "The directory relative to which the include patterns should be
       processed.")
     (include        :type (var:list-of string)
      :documentation
      "Patterns for files which should be included in the report.")
     (index-files    :type (var:list-of string)
      :documentation
      "List of files which should be presented as entry points into
       the report.")
     (keep-all?      :type boolean
      :documentation
      "Controls whether reports assembled for previous builds should
       be kept.")
     (allow-missing? :type boolean
      :documentation
      "Controls whether a missing report should make the build
       fail."))
  "Adds a publisher for arbitrary HTML-based reports."
  (let ((html (ensure-interface (jenkins.api:publishers job)
                  (jenkins.api:publisher/html))))
    (push (make-instance 'jenkins.api:html-report
                         :name           name
                         :base-directory base-directory
                         :include        include
                         :index-files    index-files
                         :keep-all?      keep-all?
                         :allow-missing? allow-missing?)
          (jenkins.api:reports html))))
