;;;; job.lisp --- Job model class.
;;;;
;;;; Copyright (C) 2012-2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.api)

;;; `job' class
;;;
;;; Aggregated classes:
;;;
;;; * `scm'
;;; * `properties'
;;; * `trigger'
;;; * `builder'
;;; * `publisher'

(define-enum-type git-browser
  (:redmine-web "hudson.plugins.git.browser.RedmineWeb")
  (:github-web  "hudson.plugins.git.browser.GithubWeb")
  (:gitea       "org.jenkinsci.plugin.gitea.GiteaBrowser"))

(define-enum-type subversion-checkout-strategy
  (:fresh-copy         "hudson.scm.subversion.CheckoutUpdater")
  (:update             "hudson.scm.subversion.UpdateUpdater")
  (:emulate-fresh-copy "hudson.scm.subversion.UpdateWithCleanUpdater"))

(define-enum-type mercurial-revision-type
  (:branch "BRANCH")
  (:tag    "TAG"))

(define-interface-implementations (scm
                                   :class-location (xloc:val "@class"))
  ((svn "hudson.scm.SubversionSCM"
        :plugin "subversion@1.43")
   ((url               :type     string
                       :xpath    "locations/hudson.scm.SubversionSCM_-ModuleLocation/remote/text()")
    (credentials       :type     string
                       :xpath    "locations/hudson.scm.SubversionSCM_-ModuleLocation/credentialsId/text()"
                       :initform nil)
    (local-directory   :type     string
                       :xpath    "locations/hudson.scm.SubversionSCM_-ModuleLocation/local/text()"
                       :initform ".")
    (checkout-strategy :type     subversion-checkout-strategy
                       :xpath    "workspaceUpdater/@class"
                       :initform :fresh-copy))
   (:name-slot url))

  ((git "hudson.plugins.git.GitSCM"
        :plugin "git@2.0")
   ((url                    :type     string
                            :xpath    "userRemoteConfigs/hudson.plugins.git.UserRemoteConfig/url/text()")
    (credentials            :type     string
                            :xpath    "userRemoteConfigs/hudson.plugins.git.UserRemoteConfig/credentialsId/text()"
                            :initform nil)
    (branches               :type     (singleton-element "name/text()")
                            :xpath    ("branches/hudson.plugins.git.BranchSpec"
                                       :if-multiple-matches :all))
    (local-branch           :type     string
                            :xpath    (:version
                                       ("git@1.1.26"
                                        "localBranch/text()")
                                       (t
                                        "extensions/hudson.plugins.git.extensions.impl.LocalBranch/localBranch/text()"))
                            :initform nil)
    (clone-timeout          :type     integer
                            :xpath    "extensions/hudson.plugins.git.extensions.impl.CloneOption/timeout/text()"
                            :initform nil)
    (wipe-out-workspace?    :type     (boolean/element "hudson.plugins.git.extensions.impl.WipeWorkspace")
                            :xpath    (:version
                                       ("git@1.1.26"
                                        "wipeOutWorkspace/text()")
                                       (t
                                        "extensions")))
    (clean-before-checkout? :type     (boolean/element "hudson.plugins.git.extensions.impl.CleanBeforeCheckout")
                            :xpath    "extensions")
    (checkout-submodules?   :type     boolean
                            :xpath    (:version
                                       ("git@1.1.26"
                                        "recursiveSubmodules/text()")
                                       (t
                                        "extensions/hudson.plugins.git.extensions.impl.SubmoduleOption/recursiveSubmodules/text()")))
    (shallow?               :type     boolean
                            :xpath    (:version
                                       ("git@1.1.26"
                                        "useShallowClone/text()")
                                       (t
                                        "extensions/hudson.plugins.git.extensions.impl.CloneOption/shallow/text()")))
    (internal-tag?          :type     (boolean/element "hudson.plugins.git.extensions.impl.PerBuildTag")
                            :xpath    (:version
                                       ("git@1.1.26"
                                        "skipTag/text()")
                                       (t
                                        "extensions")))
    (browser-kind           :type     git-browser
                            :xpath    "browser/@class"
                            :initform nil)
    (browser-url            :type     string
                            :xpath    "browser/url/text()"
                            :initform nil))
   (:name-slot url))

  ((bzr "hudson.plugins.bazaar.BazaarSCM")
   ((url :type  string
         :xpath "source/text()"))
   (:name-slot url))

  ((mercurial "hudson.plugins.mercurial.MercurialSCM"
              :plugin "mercurial@1.54")
   ((url           :type     string
                   :xpath    "source/text()")
    (credentials   :type     string
                   :xpath    "credentialsId/text()"
                   :initform nil)
    (revision-type :type     mercurial-revision-type
                   :xpath    "revisionType/text()"
                   :initform :branch)
    (branch        :type     string
                   :xpath    "revision/text()")
    (sub-directory :type     string
                   :xpath    "subdir/text()"
                   :initform nil)
    (clean?        :type     boolean
                   :xpath    "clean/text()"
                   :initform nil)
    (modules       :type     (list/space string)
                   :xpath    ("modules/text()"
                              :if-no-match :create)
                   :initform '()
                   :optional? nil))
   (:name-slot url))

  ((null "hudson.scm.NullSCM")
   ()
   (:name-slot nil)))

(defmethod credentials ((scm scm/null))
  '())

;;; property interface

(deftype cons/parameter-definition ()
  'cons)

(defmethod xloc:xml-> ((value stp:element)
                       (type  (eql 'cons/parameter-definition))
                       &key &allow-other-keys)
  (xloc:with-locations-r/o (((:name class) ".")
                            (name          "name/text()")
                            (description   "description/text()"
                                           :if-no-match :do-nothing)
                            (default       "defaultValue/text()"
                                           :if-no-match :do-nothing))
    value
    (list* :kind (switch (class :test #'string=)
                         ("hudson.model.TextParameterDefinition"   :text)
                         ("hudson.model.StringParameterDefinition" :string)
                         (t                                        class))
           :name name
           (append
            (when description (list :description description))
            (when default     (list :default default))))))

(defmethod xloc:->xml ((value list)
                       (dest  stp:element)
                       (type  (eql 'cons/parameter-definition))
                       &key &allow-other-keys)
  (xloc:with-locations (((:name class) ".")
                        (name1          "name/text()")
                        (description1   "description/text()")
                        (default1       "defaultValue/text()"))
    dest
    (let+ (((&key kind name description default) value))
          (setf class (case kind
                        (:text   "hudson.model.TextParameterDefinition")
                        (:string "hudson.model.StringParameterDefinition")
                        (t       kind))
                name1        name
                description1 (or description "")
                default1     (or default ""))))
  dest)

(define-interface-implementations (property)
  ((discard-builds "jenkins.model.BuildDiscarderProperty")
   ((keep-builds/days     :type     (or (eql -1) positive-integer)
                          :xpath    "strategy[@class=\"hudson.tasks.LogRotator\"]/daysToKeep/text()"
                          :initform -1)
    (keep-builds/count    :type     (or (eql -1) positive-integer)
                          :xpath    "strategy[@class=\"hudson.tasks.LogRotator\"]/numToKeep/text()"
                          :initform -1)
    (keep-artifacts/days  :type     (or (eql -1) positive-integer)
                          :xpath    "strategy[@class=\"hudson.tasks.LogRotator\"]/artifactDaysToKeep/text()"
                          :initform -1)
    (keep-artifacts/count :type     (or (eql -1) positive-integer)
                          :xpath    "strategy[@class=\"hudson.tasks.LogRotator\"]/artifactNumToKeep/text()"
                          :initform -1))
   (:name-slot nil))

  ((envinject "EnvInjectJobProperty"
              :plugin "envinject@1.83")
   ((properties :type     (plist/equals list/newline keyword string)
                :xpath    "info/propertiesContent/text()"
                :initform '()))
   (:name-slot nil))

  ((parameters "hudson.model.ParametersDefinitionProperty")
   ((parameters :type     cons/parameter-definition
                :xpath    ("parameterDefinitions/*"
                           :if-multiple-matches :all)
                :initform '()))
   (:name-slot nil))

  ((github "com.coravy.hudson.plugins.github.GithubProjectProperty"
           :plugin "github@1.17.1")
   ((project-url  :type     string
                  :xpath    "projectUrl/text()")
    (display-name :type     string
                  :xpath    "displayName/text()"
                  :initform nil)))

  ((docker "it.dockins.dockerslaves.spec.ContainerSetDefinition"
           :plugin "docker-slaves@1.0.5")
   ((image       :type string
                 :xpath "buildHostImage[@class=\"it.dockins.dockerslaves.spec.ImageIdContainerDefinition\"]/image/text()")
    (force-pull? :type boolean
                 :xpath "buildHostImage[@class=\"it.dockins.dockerslaves.spec.ImageIdContainerDefinition\"]/forcePull/text()"))
   (:name-slot image)))

;;; trigger interface

(define-interface-implementations (trigger)
  ((scm "hudson.triggers.SCMTrigger")
   ((spec                      :type     string)
    (ignore-post-commit-hooks? :type     boolean
                               :xpath    "ignorePostCommitHooks/text()"
                               :initform nil))
   (:name-slot spec))

  ((timer "hudson.triggers.TimerTrigger")
   ((spec :type string))
   (:name-slot spec))

  ((github "com.cloudbees.jenkins.GitHubPushTrigger"
           :plugin "github@1.4")
   ((spec :type string))
   (:name-slot spec))

  ((reverse "jenkins.triggers.ReverseBuildTrigger")
   ((spec              :type     string ; seems to be unused in Jenkins
                       :xpath    "spec/text()"
                       :initform "")
    (upstream-projects :type     (list/comma string)
                       :xpath    "upstreamProjects/text()"
                       :initform '())
    (threshold         :type     stupid-threshold
                       :xpath    "threshold"
                       :initform :success))
   (:name-slot upstream-projects)))

;;; build-wrapper interface

(define-interface-implementations (build-wrapper)
  ((timeout "hudson.plugins.build__timeout.BuildTimeoutWrapper"
            :plugin "build-timeout@1.11")
   ((kind            :type     (keyword/downcase :absolute)
                     :xpath    "timeoutType/text()"
                     :initform :absolute)
    (timeout/minutes :type     real
                     :xpath    "timeoutMinutes/text()")
    (fail-build?     :type     boolean
                     :xpath   "failBuild/text()"
                     :initform nil))
   (:name-slot kind)))

;;; builder interface

(macrolet
    ((define-maven-settings-type (name default-provider file-path-provider)
       `(progn
          (deftype ,name ()
            '(or (eql :default) string))

          (defmethod xloc:xml-> ((value stp:element)
                                 (type  (eql ',name))
                                 &key &allow-other-keys)
            (xloc:with-locations-r/o (((:@ class) ".")
                                      (path       "path/text()" :if-no-match :do-nothing))
              value
              (cond
                ((string= class ,default-provider)
                 :default)
                ((string= class ,file-path-provider)
                 path))))

          (defmethod xloc:->xml ((value t)
                                 (dest  stp:element)
                                 (type  (eql ',name))
                                 &key &allow-other-keys)
            (xloc:with-locations (((:@ class) ".")
                                  (path       "path/text()"))
              dest
              (etypecase value
                ((eql :default)
                 (setf class ,default-provider)
                 (stp:delete-children dest))
                (string
                 (setf class ,file-path-provider)
                 (setf path value))))
            dest))))

  (define-maven-settings-type maven-settings
    "jenkins.mvn.DefaultSettingsProvider"
    "jenkins.mvn.FilePathSettingsProvider")

  (define-maven-settings-type maven-global-settings
    "jenkins.mvn.DefaultGlobalSettingsProvider"
    "jenkins.mvn.FilePathGlobalSettingsProvider"))

(define-interface-implementations (builder)
  ((shell "hudson.tasks.Shell")
   ((command :type  string))
   (:name-slot command))

  ((batch "hudson.tasks.BatchFile")
   ((command :type  string))
   (:name-slot command))

  ((cmake "hudson.plugins.cmake.CmakeBuilder")
   ((command :type  string
             :xpath "makeCommand/text()"))
   (:name-slot command))

  ((ant "hudson.tasks.Ant"
        :plugin "ant@1.1")
   ((targets    :type  string)
    (properties :type  string))
   (:name-slot targets))

  ((maven "hudson.tasks.Maven")
   ((targets             :type     (list/space string)
                         :initform '())
    (properties          :type     (equals+newline/plist keyword string)
                         :initform '())
    (private-repository? :type     boolean
                         :xpath    "usePrivateRepository/text()"
                         :initform nil)
    (settings            :type     maven-settings
                         :xpath    "settings"
                         :initform :default)
    (global-settings     :type     maven-global-settings
                         :xpath    "globalSettings"
                         :initform :default))
   (:name-slot targets))

  ((copy-artifact "hudson.plugins.copyartifact.CopyArtifact"
                  :plugin "copyartifact@1.27")
   ((project-name :type  string
                  :xpath (:version
                          ("copyartifact@1.25" "projectName/text()")
                          (t                   "project/text()")))
    (filter       :type  string
                  :optional? t)
    (target       :type  string)
    (flatten?     :type  boolean
                  :xpath "flatten/text()")
    ;; TODO(jmoringe, 2012-12-13): temp
    (clazz        :type  string
                  :xpath "selector/@class"))
   (:name-slot project-name))

  ((groovy "hudson.plugins.groovy.Groovy"
           :plugin "groovy@2.0")
   ((code :type  string
          :xpath "scriptSource[@class=\"hudson.plugins.groovy.StringScriptSource\"]/command/text()"))
   (:name-slot code))

  ((system-groovy "hudson.plugins.groovy.SystemGroovy"
                  :plugin "groovy@2.0")
   ((code     :type     string
              :xpath    "source[@class=\"hudson.plugins.groovy.StringSystemScriptSource\"]/script[@plugin=\"script-security@1.27\"]/script/text()")
    (sandbox? :type     boolean
              :xpath    "source[@class=\"hudson.plugins.groovy.StringSystemScriptSource\"]/script[@plugin=\"script-security@1.27\"]/sandbox/text()"
              :initform nil))
   (:name-slot code)))

;;; publisher interface

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
   ((pattern :type string))
   (:name-slot pattern))

  ((cobertura "hudson.plugins.cobertura.CoberturaPublisher"
              :plugin "cobertura@1.7.1")
   ((report-file :type  string
                 :xpath "coberturaReportFile/text()"))
   (:name-slot report-file))

  ((html "htmlpublisher.HtmlPublisher"
         :plugin "htmlpublisher@1.2")
   ()
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

(define-model-class job ()
  ((description                :type     string)
   (disabled?                  :type     boolean
                               :xpath    "disabled/text()")
   (block-on-downstream-build? :type     boolean
                               :xpath    "blockBuildWhenDownstreamBuilding/text()")
   (block-on-upstream-build?   :type     boolean
                               :xpath    "blockBuildWhenUpstreamBuilding/text()")
   (can-roam?                  :type     boolean
                               :xpath    "canRoam/text()"
                               :initform t)
   (restrict-to-slaves         :type     string
                               :xpath    "assignedNode/text()"
                               :optional? t)
   ;; Interface-based children
   (properties                 :type     property
                               :xpath    ("properties/*"
                                          :if-multiple-matches :all))
   (triggers                   :type     trigger
                               :xpath    ("triggers/*"
                                          :if-multiple-matches :all)
                               ;; TODO :initform '()
                               )
   (repository                 :type     scm
                               :xpath    ("scm"))
   (build-wrappers             :type     build-wrapper
                               :xpath    ("buildWrappers/*"
                                          :if-multiple-matches :all)
                               ;; TODO :initform '()
                               )
   (builders                   :type     builder
                               :xpath    ("builders/*"
                                          :if-multiple-matches :all)
                               ;; TODO :initform '()
                               )
   (publishers                 :type     publisher
                               :xpath    ("publishers/*"
                                          :if-multiple-matches :all)
                               ;; TODO :initform '()
                               )

   ;; TODO Not sure about these
   (slaves          :type     string/node ; TODO(jmoringe, 2012-07-10): not correct
                    :xpath    ("axes/hudson.matrix.LabelAxis[name/text()='label']/values/string"
                               :if-multiple-matches :all)
                    :optional? t)

   (environment     :type     (equals+newline/plist keyword string)
                    :xpath    "buildWrappers/hudson.plugins.setenv.SetEnvBuildWrapper/localVarText/text()")

   (permissions     :type     access-control-rule
                    :xpath    ("properties/hudson.security.AuthorizationMatrixProperty/permission"
                               :if-multiple-matches :all))
   (jdk             :type     string
                    :xpath    "jdk/text()")
   ;; TODO these will be moved into the appropriate interfaces
   (redmine-instance :type     string
                     :xpath    #+TODO (:version
                                       ("redmine@0.14"
                                        "properties/hudson.plugins.redmine.RedmineProjectProperty/redmineWebsite/text()")
                                       (t
                                        "properties/hudson.plugins.redmine.RedmineProjectProperty/redmineWebsiteName/text()"))
                               "properties/hudson.plugins.redmine.RedmineProjectProperty/redmineWebsiteName/text()")
   (redmine-version :type     string
                    :xpath    "properties/hudson.plugins.redmine.RedmineProjectProperty/redmineVersionNumber/text()")
   (redmine-project :type     string
                    :xpath    "properties/hudson.plugins.redmine.RedmineProjectProperty/projectName/text()"))
  (:get-func (lambda (id)      (job-config id)))
  (:put-func (lambda (id data) (setf (job-config id) data))))

(defun job-name-character? (character)
  (or (alphanumericp character) (member character '(#\- #\_ #\.))))

(defun job-name? (name)
  (every #'job-name-character? name))

(deftype job-name ()
  '(satisfies job-name?))

(defun check-job-name (name)
  (unless (job-name? name)
    (let ((offenders (map 'list (lambda (char)
                                  (list char (char-name char)))
                          (remove-duplicates
                           (remove-if #'job-name-character? name)))))
      (error 'simple-type-error
             :datum            name
             :expected-type    'job-name
             :format-control   "~@<Supplied job name ~S contains illegal ~
                                character~P: ~{~{~A (~@[~A~])~}~^, ~}.~@:>"
             :format-arguments (list name (length offenders) offenders)))))

(defmethod initialize-instance :before ((instance job)
                                        &key
                                        id
                                        check-id?)
  (when check-id? (check-job-name id)))

(defmethod kind ((object job))
  (stp:local-name (stp:document-element (%data object))))

(defmethod (setf kind) ((new-value (eql :project))
                        (object    job))
  (setf (kind object) "project"))

(defmethod (setf kind) ((new-value (eql :matrix))
                        (object    job))
  (setf (kind object) '("matrix-project" "matrix-project@1.4")))

(defmethod (setf kind) ((new-value string)
                        (object    job))
  (setf (kind object) (list new-value))
  new-value)

(defmethod (setf kind) ((new-value cons)
                        (object    job))
  (let+ (((local-name &optional plugin) new-value)
         (root (stp:document-element (%data object))))
        (setf (stp:local-name root) local-name)
        (when plugin
          (setf (stp:attribute-value root "plugin") plugin))
        new-value))

(macrolet ((define-of-type-methods (interface
                                    &optional
                                    (plural (symbolicate interface '#:s)))
             (let ((name/list (symbolicate plural    '#:-of-type))
                   (name/one  (symbolicate interface '#:-of-type)))
               `(progn
                  (defmethod ,name/list (type job)
                    (remove-if-not (of-type type) (,plural job)))

                  (defmethod ,name/one (type job)
                    (find-if (of-type type) (,plural job)))))))

  (define-of-type-methods property properties)
  (define-of-type-methods trigger)
  (define-of-type-methods build-wrapper)
  (define-of-type-methods builder)
  (define-of-type-methods publisher))

(defmethod upstream ((object job))
  (when-let ((reverse (trigger-of-type 'trigger/reverse object)))
            (upstream-projects reverse)))

(defmethod (setf upstream) ((new-value list) (object job))
  (let ((reverse (or (trigger-of-type 'trigger/reverse object) ; TODO make a function or macro ensure-...
                     (let ((instance (make-instance 'trigger/reverse)))
                       (appendf (triggers object) (list instance))
                       instance))))
    (setf (upstream-projects reverse) new-value)))

;;; Permissions

(defmethod grant ((job job) (subject string) (action cons))
  (pushnew (list subject action) (permissions job) :test #'equal)
  (permissions job))

(defmethod revoke ((job job) (subject string) (action cons))
  (removef (permissions job) (list subject action) :test #'equal)
  (permissions job))

(macrolet
    ((define-permission-methods (name)
       `(progn
          (defmethod ,name ((job string) (subject t) (action t))
            (,name (job job) subject action))

          (defmethod ,name ((job job) (subject list) (action t))
            (mapc #'(lambda (subject) (,name job subject action)) subject)
            (permissions job)))))

  (define-permission-methods grant)
  (define-permission-methods revoke))
