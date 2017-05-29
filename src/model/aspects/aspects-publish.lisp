;;;; aspects-publish.lisp --- Definitions of publisher-creating aspects
;;;;
;;;; Copyright (C) 2012-2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.model.aspects)

;;; Tasks aspect

(define-aspect (tasks) (publisher-defining-mixin)
    ((pattern               :type (list-of string)
      :documentation
      "Filename patterns specifying which workspace files to scan for
       open tasks.")
     ((exclude         '()) :type (list-of string)
      :documentation
      "Filename patterns specifying which workspace files to exclude
       from the scan for open tasks.")
     ((keywords.low    '()) :type (list-of string)
      :documentation
      "Keywords indicating low-priority open tasks.")
     ((keywords.normal '()) :type (list-of string)
      :documentation
      "Keywords indicating normal-priority open tasks.")
     ((keywords.high   '()) :type (list-of string)
      :documentation
      "Keywords indicating high-priority open tasks."))
  "Adds an open tasks publisher to the generated job."
  (push (constraint! (publish)
          (tasks (:pattern         pattern
                  :exclude         exclude
                  :keywords/low    keywords.low
                  :keywords/normal keywords.normal
                  :keywords/high   keywords.high)))
        (publishers job)))

;;; SLOCcount aspect

(define-aspect (sloccount) (builder-defining-mixin
                            publisher-defining-mixin)
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
            (shell (:command command)))
          (builders job)))

  (push (constraint! (publish) (sloccount (:pattern "sloccount.sc")))
        (publishers job)))

;;; Warnings aspect

(define-aspect (warnings :job-var job) (publisher-defining-mixin)
    ((parsers :type (list-of string)
      :documentation
      "Names of parsers to apply to the output of the generated job.

       Parsers can be either builtin or defined in the global Jenkins
       configuration."))
  "Configures a warnings publisher for the generated job."
  (when parsers
    (with-interface (publishers job) (warnings (publisher/warnings))
      (iter (for parser in parsers)
            (pushnew (make-instance 'warning-parser/console :name parser)
                     (console-parsers warnings)
                     :test #'string=
                     :key  #'jenkins.api:name)))))

;;; Checkstyle and PMD aspects

(macrolet ((define (name publisher-name display-name)
             `(define-aspect (,name :job-var job) (publisher-defining-mixin)
                  ((pattern :type list
                    :documentation
                    "Analysis results should be read from files
                     matching the pattern."))
                ,(format nil "Configures a ~A publisher for the generated job."
                         display-name)
                (removef (publishers job) ',publisher-name :key #'type-of)
                (when pattern
                  (push (constraint! (publish)
                          (make-instance ',publisher-name :pattern pattern))
                        (publishers job))))))
  (define checkstyle publisher/checkstyle "CheckStyle")
  (define pmd        publisher/pmd        "PMD"))

;;; Test result aspects

(define-aspect (xunit :job-var job) (publisher-defining-mixin)
    ((kind                      :type string)
     (pattern                   :type (or null string))
     (skip-if-no-test-files?    :type boolean)
     (fail-if-not-new?          :type boolean)
     (delete-output-files?      :type boolean)
     (stop-processing-if-error? :type boolean))
  "Configures a publisher for XUnit test results for the generated job."
  (with-interface (publishers job) (publisher (publisher/xunit))
    (removef (types publisher) kind :test #'string= :key #'kind)
    (when pattern
      (push (make-instance
             'xunit/type
             :kind                      kind
             :pattern                   pattern
             :skip-if-no-test-files?    skip-if-no-test-files?
             :fail-if-not-new?          fail-if-not-new?
             :delete-output-files?      delete-output-files?
             :stop-processing-if-error? stop-processing-if-error?)
            (types publisher)))))

(define-aspect (junit :job-var job) (publisher-defining-mixin)
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
  (removef (publishers job) 'publisher/junit :key #'type-of)
  ;; Add new configuration.
  (when pattern
    (push (constraint! (publish)
            (make-instance 'publisher/junit
                           :pattern              pattern
                           :keep-long-stdio?     keep-long-stdio?
                           :health-scale-factor  health-scale-factor
                           :allow-empty-results? allow-empty-results?))
          (publishers job))))

;;; Email notification

(define-aspect (email-notification :job-var job) (publisher-defining-mixin)
    ((recipients           :type (list-of string)
      :documentation
      "A list of email addresses to which notifications in case of a
       build failure should be sent.")
     (send-to-perpetrator? :type boolean
      :documentation
      "Controls whether the authors of build-breaking commits are
       treated as additional recipients."))
  "Adds email notification in case of failed builds to a generated job."
  (removef (publishers job) 'publisher/email-notification :key #'type-of)
  (when recipients
    (push (constraint! (publish)
            (make-instance 'publisher/email-notification
                           :recipients           recipients
                           :send-to-individuals? send-to-perpetrator?))
          (publishers job))))

;;; Upload aspect

(define-aspect (upload :job-var job) (publisher-defining-mixin)
    ((target             :type string
      :documentation
      "The name of the machine to which artifacts should be uploaded.")
     (source-files       :type (list-of string)
      :documentation
      "List of files to upload.")
     ((excludes     '()) :type (list-of string)
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
          (ssh (:target           target
                :source-files     source-files
                :excludes         excludes
                :remove-prefix    remove-prefix
                :remote-directory remote-directory
                :verbose?         verbose?)))
        (publishers job)))

;;; HTML report aspect

(define-aspect (html-report :job-var job) (publisher-defining-mixin)
    ((name           :type string
      :documentation
      "Name of the report.")
     (base-directory :type string
      :documentation
      "The directory relative to which the include patterns should be
       processed.")
     (include        :type (list-of string)
      :documentation
      "Patterns for files which should be included in the report.")
     (index-files    :type (list-of string)
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
  (let ((html (ensure-interface (publishers job) (publisher/html))))
    (push (make-instance 'html-report :name           name
                                      :base-directory base-directory
                                      :include        include
                                      :index-files    index-files
                                      :keep-all?      keep-all?
                                      :allow-missing? allow-missing?)
          (reports html))))
