;;;; job.lisp --- Job model class.
;;;;
;;;; Copyright (C) 2012-2019 Jan Moringen
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
                                   :selectors?     nil
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
                            :xpath    "extensions/hudson.plugins.git.extensions.impl.LocalBranch/localBranch/text()"
                            :initform nil)
    (clone-timeout          :type     integer
                            :xpath    "extensions/hudson.plugins.git.extensions.impl.CloneOption/timeout/text()"
                            :initform nil)
    (wipe-out-workspace?    :type     (boolean/element "hudson.plugins.git.extensions.impl.WipeWorkspace")
                            :xpath    "extensions")
    (clean-before-checkout? :type     (boolean/element "hudson.plugins.git.extensions.impl.CleanBeforeCheckout")
                            :xpath    "extensions")
    (checkout-submodules?   :type     boolean
                            :xpath    "extensions/hudson.plugins.git.extensions.impl.SubmoduleOption/recursiveSubmodules/text()")
    (shallow?               :type     boolean
                            :xpath    "extensions/hudson.plugins.git.extensions.impl.CloneOption/shallow/text()")
    (internal-tag?          :type     (boolean/element "hudson.plugins.git.extensions.impl.PerBuildTag")
                            :xpath    "extensions")
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

(define-model-class job ()
  ((description                :type     string
                               :initform nil)
   (disabled?                  :type     boolean
                               :xpath    "disabled/text()"
                               :initform nil)
   (block-on-downstream-build? :type     boolean
                               :xpath    "blockBuildWhenDownstreamBuilding/text()"
                               :initform nil)
   (block-on-upstream-build?   :type     boolean
                               :xpath    "blockBuildWhenUpstreamBuilding/text()"
                               :initform nil)
   (can-roam?                  :type     boolean
                               :xpath    "canRoam/text()"
                               :initform t)
   (restrict-to-slaves         :type     string
                               :xpath    "assignedNode/text()"
                               :initform nil
                               :optional? t)
   ;; Interface-based children
   (properties                 :type     property
                               :xpath    ("properties/*"
                                          :if-multiple-matches :all)
                               :initform '())
   (triggers                   :type     trigger
                               :xpath    ("triggers/*"
                                          :if-multiple-matches :all)
                               :initform '())
   (repository                 :type     scm
                               :xpath    ("scm")
                               :initform nil)
   (build-wrappers             :type     build-wrapper
                               :xpath    ("buildWrappers/*"
                                          :if-multiple-matches :all)
                               :initform '())
   (builders                   :type     builder
                               :xpath    ("builders/*"
                                          :if-multiple-matches :all)
                               :initform '())
   (publishers                 :type     publisher
                               :xpath    ("publishers/*"
                                          :if-multiple-matches :all)
                               :initform '())

   ;; TODO Not sure about these
   (slaves          :type     string/node ; TODO(jmoringe, 2012-07-10): not correct
                    :xpath    ("axes/hudson.matrix.LabelAxis[name/text()='label']/values/string"
                               :if-multiple-matches :all)
                    :optional? t
                    :initform '())
   (permissions     :type     access-control-rule
                    :xpath    ("properties/hudson.security.AuthorizationMatrixProperty/permission"
                               :if-multiple-matches :all)
                    :initform '())
   (jdk             :type     string
                    :xpath    "jdk/text()"
                    :initform nil))
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

(defmethod shared-initialize :around ((instance   job)
                                      (slot-names t)
                                      &rest args &key
                                      populate?)
  ;; Only initialize all slots if POPULATE? is true. Otherwise, the
  ;; uninitialized slots will be populated lazily from the remote
  ;; state.
  (if populate?
      (call-next-method)
      (apply #'call-next-method instance '(id get-func put-func) args)))

(defmethod initialize-instance :before ((instance job)
                                        &key
                                        id
                                        check-id?
                                        kind
                                        populate?)
  (when check-id? (check-job-name id))
  ;; Setting the kind requires the `%data' slot to be initialized
  ;; which is only the case when POPULATE? is true.
  (when (and kind (not populate?))
    (incompatible-initargs 'job :kind kind :populate? populate?)))

(defmethod initialize-instance :after ((instance job)
                                       &key
                                       kind
                                       populate?)
  ;; When POPULATE? is true, all slots are initialized to default
  ;; values. Put an empty document into the `%data' slot to match that
  ;; state.
  (when populate?
    (setf (%data instance) (stp:make-document (stp:make-element "project")))
    (when kind
      (setf (kind instance) kind))))

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
