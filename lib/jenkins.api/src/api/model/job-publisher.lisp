;;;; job-publisher.lisp --- Model classes for publisher implementations .
;;;;
;;;; Copyright (C) 2012-2021, 2024 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.api)

;;; Classic warnings

(define-model-class warning-parser/file ()
  ((pattern :type  string
            :xpath "pattern/text()")
   (name    :type  string
            :xpath "parserName/text()"))
  (:name-slot pattern))

(define-model-class warning-parser/console ()
  ((name :type  string
         :xpath "parserName/text()"))
  (:name-slot name))

;;; XUnit

(define-model-class xunit/type ()
  ((kind                      :type     string
                              :xpath    nil)
   (pattern                   :type     string
                              :xpath    "pattern/text()")
   (skip-if-no-test-files?    :type     boolean
                              :xpath    "skipNoTestFiles/text()"
                              :initform nil)
   (fail-if-not-new?          :type     boolean
                              :xpath    "failIfNotNew/text()"
                              :initform t)
   (delete-output-files?      :type     boolean
                              :xpath    "deleteOutputFiles/text()"
                              :initform t)
   (stop-processing-if-error? :type     boolean
                              :xpath    "stopProcessingIfError/text()"
                              :initform t))
  (:name-slot kind))

(defmethod xloc:xml-> :around ((value stp:element)
                               (type  xunit/type)
                               &key &allow-other-keys)
  (let* ((result        (call-next-method))
         (type          (stp:local-name value))
         (type/stripped (if (ends-with-subseq "Type" type)
                            (subseq type 0 (- (length type) 4))
                            type)))
    (setf (slot-value result 'kind) type/stripped)
    result))

(defmethod xloc:->xml :around ((value xunit/type)
                               (dest  stp:element)
                               (type  (eql 'xunit/type))
                               &key &allow-other-keys)
  (let ((result (call-next-method)))
    (setf (stp:local-name result)
          (format nil "~AType" (slot-value value 'kind)))
    result))

;;; Warnings NG stuff

(define-interface-implementations (analysis-tool)
  ((open-tasks "io.jenkins.plugins.analysis.warnings.tasks.OpenTasks")
   (;; Name
    (id                  :type  string
                         :xpath "id/text()")
    (name                :type  string
                         :xpath "name/text()")
    ;; Files
    (include-pattern     :type  (list/comma string)
                         :xpath "includePattern/text()")
    (exclude-pattern     :type  (list/comma string)
                         :xpath "excludePattern/text()")
    ;; Tags
    (high-tags           :type  (list/comma string)
                         :xpath "highTags/text()")
    (normal-tags         :type  (list/comma string)
                         :xpath "normalTags/text()")
    (low-tags            :type  (list/comma string)
                         :xpath "lowTags/text()")
    (ignore-case?        :type  boolean
                         :xpath "ignoreCase/text()")
    (regular-expression? :type  boolean
                         :xpath "isRegularExpression/text()")))

  ((gcc4 "io.jenkins.plugins.analysis.warnings.Gcc4")
   (;; Name
    (id              :type     string
                     :xpath    "id/text()")
    (name            :type     string
                     :xpath    "name/text")
    ;; Files
    (pattern         :type     (list/comma string)
                     :xpath    "pattern/text()"
                     :initform '())
    (report-encoding :type     string
                     :xpath    "reportEncoding/text()"
                     :initform nil)))

  ((java "io.jenkins.plugins.analysis.warnings.Java")
   (;; Name
    (id              :type     string
                     :xpath    "id/text()")
    (name            :type     string
                     :xpath    "name/text")
    ;; Files
    (pattern         :type     (list/comma string)
                     :xpath    "pattern/text()"
                     :initform '())
    (report-encoding :type     string
                     :xpath    "reportEncoding/text()"
                     :initform nil)))

  ((maven "io.jenkins.plugins.analysis.warnings.MavenConsole")
   (;; Name
    (id              :type     string
                     :xpath    "id/text()")
    (name            :type     string
                     :xpath    "name/text")
    ;; Files
    (pattern         :type     (list/comma string)
                     :xpath    "pattern/text()"
                     :initform '())
    (report-encoding :type     string
                     :xpath    "reportEncoding/text()"
                     :initform nil)))

  ((checkstyle "io.jenkins.plugins.analysis.warnings.checkstyle.CheckStyle")
   (;; Name
    (id              :type     string
                     :xpath    "id/text()")
    (name            :type     string
                     :xpath    "name/text()")
    ;; Files
    (pattern         :type     (list/comma string)
                     :xpath    "pattern/text()"
                     :initform '())
    (report-encoding :type     string
                     :xpath    "reportEncoding/text()"
                     :initform nil)))

  ((pmd "io.jenkins.plugins.analysis.warnings.Pmd")
   (;; Name
    (id              :type     string
                     :xpath    "id/text()")
    (name            :type     string
                     :xpath    "name/text()")
    ;; Files
    (pattern         :type     (list/comma string)
                     :xpath    "pattern/text()"
                     :initform '())
    (report-encoding :type     string
                     :xpath    "reportEncoding/text()"
                     :initform nil)))

  ((cmake "io.jenkins.plugins.analysis.warnings.Cmake.")
   (;; Name
    (id              :type     string
                     :xpath    "id/text()")
    (name            :type     string
                     :xpath    "name/text")
    ;; Files
    (pattern         :type     (list/comma string)
                     :xpath    "pattern/text()"
                     :initform '())
    (report-encoding :type     string
                     :xpath    "reportEncoding/text()"
                     :initform nil)))

  ((groovy "io.jenkins.plugins.analysis.warnings.groovy.GroovyScript")
   (;; Name
    (id              :type     string
                     :xpath    "id/text()")
    (name            :type     string
                     :xpath    "name/text()")
    ;; Files
    (pattern         :type     (list/comma string)
                     :xpath    "pattern/text()"
                     :initform '())
    (report-encoding :type     string
                     :xpath    "reportEncoding/text()"
                     :initform nil)
    ;; Parser
    (parser          :type     string
                     :xpath    "parserId/text()"))))

;;; HTML report

(define-model-class html-report ()
  ((name           :type     string
                   :xpath    "reportName/text()")
   (base-directory :type     string
                   :xpath    "reportDir/text()")
   (include        :type     (list/comma string)
                   :xpath    "includes/text()")
   (index-files    :type     (list/comma string)
                   :xpath    "reportFiles/text()")
   (keep-all?      :type     boolean
                   :xpath    "keepAll/text()")
   (allow-missing? :type     boolean
                   :xpath    "allowMissing/text()"))
  (:name-slot name))

;;; Publisher interface

(define-interface-implementations (publisher)
  ((ssh "jenkins.plugins.publish__over__ssh.BapSshPublisherPlugin"
        :plugin "publish-over-ssh@1.10")
   ((target           :type    string
                      :xpath   "delegate/publishers/jenkins.plugins.publish__over__ssh.BapSshPublisher/configName/text()")
    (verbose?         :type    boolean
                      :xpath   "delegate/publishers/jenkins.plugins.publish__over__ssh.BapSshPublisher/verbose/text()")
    (source-files     :type    string
                      :xpath   "delegate/publishers/jenkins.plugins.publish__over__ssh.BapSshPublisher/transfers/jenkins.plugins.publish__over__ssh.BapSshTransfer/sourceFiles/text()")
    (excludes         :type    string
                      :xpath   "delegate/publishers/jenkins.plugins.publish__over__ssh.BapSshPublisher/transfers/jenkins.plugins.publish__over__ssh.BapSshTransfer/excludes/text()")
    (remove-prefix    :type    string
                      :xpath   "delegate/publishers/jenkins.plugins.publish__over__ssh.BapSshPublisher/transfers/jenkins.plugins.publish__over__ssh.BapSshTransfer/removePrefix/text()")
    (remote-directory :type    string
                      :xpath   "delegate/publishers/jenkins.plugins.publish__over__ssh.BapSshPublisher/transfers/jenkins.plugins.publish__over__ssh.BapSshTransfer/remoteDirectory/text()"))
   (:name-slot target))

  ((warnings "hudson.plugins.warnings.WarningsPublisher"
             :plugin "warnings@4.21")
   ((encoding        :type      string
                     :xpath     "defaultEncoding/text()"
                     :initform  "UTF-8")
    (file-parsers    :type      warning-parser/file
                     :xpath     ("parserConfigurations/hudson.plugins.warnings.ParserConfiguration"
                                 :if-multiple-matches :all)
                     :optional? nil
                     :initform  '())
    (console-parsers :type      warning-parser/console
                     :xpath     ("consoleParsers/hudson.plugins.warnings.ConsoleParser"
                                 :if-multiple-matches :all)
                     :optional? nil
                     :initform '()))
   (:name-slot nil))

  ((issues-recorder "io.jenkins.plugins.analysis.core.steps.IssuesRecorder"
                    :plugin "warnings-ng@8.10.1")
   ((analysis-tools          :type      analysis-tool
                             :xpath     ("analysisTools/*"
                                         :if-multiple-matches :all)
                             :initform  '())
    (filters                 :type      string
                             :xpath     ("filters/text()"
                                         :if-multiple-matches :all)
                             :optional? nil
                             :initform  '())
    ;; Control
    (is-enabled-for-failure? :type      boolean
                             :xpath     "isEnabledForFailure/text()"
                             :initform  t)
    (ignore-quality-gate?    :type      boolean
                             :xpath     "ignoreQualityGate/text()"
                             :optional? nil
                             :initform  nil)
    ;; Assessment
    (healthy-threshold       :type      non-negative-integer
                             :xpath     "healthy/text()"
                             :optional? nil
                             :initform  0)
    (unhealthy-threshold     :type      non-negative-integer
                             :xpath     "unhealthy/text()"
                             :optional? nil
                             :initform  0)
    (minimum-severity        :type      string
                             :xpath     "minimumSeverity/name/text()"
                             :optional? nil
                             :initform  "HIGH")
    ;; Blame
    (blame-disabled?         :type      boolean
                             :xpath     "isBlameDisabled/text()"
                             :optional? nil
                             :initform  nil))
   (:name-slot nil))

  ((checkstyle "hudson.plugins.checkstyle.CheckStylePublisher"
               :plugin "checkstyle@3.46")
   ((pattern :type     (list/comma string)
             :initform '()))
   (:name-slot pattern))

  ((pmd "hudson.plugins.pmd.PmdPublisher"
        :plugin "pmd@3.46")
   ((pattern :type     (list/comma string)
             :initform '()))
   (:name-slot pattern))

  ((tasks "hudson.plugins.tasks.TasksPublisher"
          :plugin "tasks@4.35")
   ((pattern         :type     (list/comma string)
                     :initform '())
    (exclude         :type     (list/comma string)
                     :xpath    "excludePattern/text()"
                     :initform '())
    (threshold-limit :type     keyword/downcase
                     :xpath    "thresholdLimit/text()"
                     :initform :low)
    (keywords/low    :type     (list/comma string)
                     :xpath    "low/text()"
                     :initform '())
    (keywords/normal :type     (list/comma string)
                     :xpath    "normal/text()"
                     :initform '())
    (keywords/high   :type     (list/comma string)
                     :xpath    "high/text()"
                     :initform '())
    (ignore-case?    :type     boolean
                     :xpath    "ignoreCase/text()"
                     :initform t))
   (:name-slot pattern))

  ((archive-artifacts "hudson.tasks.ArtifactArchiver")
   ((files        :type     (list/comma string)
                  :xpath    "artifacts/text()")
    (only-latest? :type boolean
                  :xpath    "onlyLatest/text()"))
   (:name-slot files))

  ((fingerprint "hudson.tasks.Fingerprinter")
   ((targets          :type      (list/comma string))
    (build-artifacts? :type      boolean
                      :xpath     "recordBuildArtifacts/text()"))
   (:name-slot targets))

  ((sloccount "hudson.plugins.sloccount.SloccountPublisher"
              :plugin "sloccount@1.8")
   ((pattern               :type     string)
    (ignore-build-failure? :type     boolean
                           :xpath    "ignoreBuildFailure/text()"
                           :initform t))
   (:name-slot pattern))

  ((cobertura "hudson.plugins.cobertura.CoberturaPublisher"
              :plugin "cobertura@1.7.1")
   ((report-file :type  string
                 :xpath "coberturaReportFile/text()"))
   (:name-slot report-file))

  ((html "htmlpublisher.HtmlPublisher"
         :plugin "htmlpublisher@1.12")
   ((reports :type     html-report
             :xpath    ("reportTargets/htmlpublisher.HtmlPublisherTarget"
                        :if-multiple-matches :all)
             :initform '()))
   (:name-slot nil))

  ((ci-game "hudson.plugins.cigame.GamePublisher"
            :plugin "ci-game@1.19")
   ()
   (:name-slot nil))

  ((email-notification "hudson.tasks.Mailer"
                       :plugin "mailer@1.16")
   ((recipients            :type     (list/space string))
    (every-unstable-build? :type     boolean
                           :xpath    "dontNotifyEveryUnstableBuild/text()"
                           :initform t)
    (send-to-individuals?  :type     boolean
                           :xpath    "sendToIndividuals/text()"
                           :initform nil))
   (:name-slot recipients))

  ((mailer/extended "hudson.plugins.emailext.ExtendedEmailPublisher"
                    :plugin "email-ext@2.25")
   ()
   (:name-slot nil))

  ((blame-upstream "hudson.plugins.blame__upstream__commiters.BlameUpstreamCommitersPublisher"
                   :plugin "blame-upstream-commiters@1.2")
   ((send-to-individuals? :type  boolean
                          :xpath "sendtoindividuals/text()"))
   (:name-slot nil))

  ((xunit "xunit" :plugin "xunit@1.93")
   ((types :type     xunit/type
           :xpath    ("types/*" :if-multiple-matches :all)
           :initform '()))
   (:name-slot nil))

  ((junit "hudson.tasks.junit.JUnitResultArchiver"
          :plugin "junit@1.4")
   ((pattern              :type     string
                          :xpath    "testResults/text()")
    (keep-long-stdio?     :type     boolean
                          :xpath    "keepLongStdio/text()"
                          :initform nil)
    (health-scale-factor  :type     health-scale-factor
                          :xpath    "healthScaleFactor/text()"
                          :initform 1.0)
    (allow-empty-results? :type     boolean
                          :xpath    "allowEmptyResults/text()"
                          :initform nil))
   (:name-slot pattern)))
