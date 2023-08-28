;;;; job-scm.lisp --- Model classes for SCM implementations.
;;;;
;;;; Copyright (C) 2012-2019, 2023 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.api)

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
  ((svn "hudson.scm.SubversionSCM" :plugin "subversion@1.43")
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

  ((git "hudson.plugins.git.GitSCM" :plugin "git@2.0")
   ((url                         :type     string
                                 :xpath    "userRemoteConfigs/hudson.plugins.git.UserRemoteConfig/url/text()")
    (credentials                 :type     string
                                 :xpath    "userRemoteConfigs/hudson.plugins.git.UserRemoteConfig/credentialsId/text()"
                                 :initform nil)
    (branches                    :type     (singleton-element "name/text()")
                                 :xpath    ("branches/hudson.plugins.git.BranchSpec"
                                            :if-multiple-matches :all))
    (local-branch                :type     string
                                 :xpath    "extensions/hudson.plugins.git.extensions.impl.LocalBranch/localBranch/text()"
                                 :initform nil)
    (clone-timeout               :type     integer
                                 :xpath    "extensions/hudson.plugins.git.extensions.impl.CloneOption/timeout/text()"
                                 :initform nil)
    (wipe-out-workspace?         :type     (boolean/element "hudson.plugins.git.extensions.impl.WipeWorkspace")
                                 :xpath    "extensions")
    (clean-before-checkout?      :type     (boolean/element "hudson.plugins.git.extensions.impl.CleanBeforeCheckout")
                                 :xpath    "extensions")
    (checkout-submodules?        :type     boolean
                                 :xpath    "extensions/hudson.plugins.git.extensions.impl.SubmoduleOption/recursiveSubmodules/text()")
    (shallow?                    :type     boolean
                                 :xpath    "extensions/hudson.plugins.git.extensions.impl.CloneOption/shallow/text()")
    (poll-ignore-include-regions :type     (list/newline string)
                                 :xpath    "extensions/hudson.plugins.git.extensions.impl.PathRestriction/includedRegions/text()"
                                 :initform '())
    (poll-ignore-exclude-regions :type     (list/newline string)
                                 :xpath    "extensions/hudson.plugins.git.extensions.impl.PathRestriction/excludedRegions/text()"
                                 :initform '())
    (lfs?                        :type     (boolean/element "hudson.plugins.git.extensions.impl.GitLFSPull")
                                 :xpath    "extensions")
    (internal-tag?               :type     (boolean/element "hudson.plugins.git.extensions.impl.PerBuildTag")
                                 :xpath    "extensions")
    (browser-kind                :type     git-browser
                                 :xpath    "browser/@class"
                                 :initform nil)
    (browser-url                 :type     string
                                 :xpath    "browser/url/text()"
                                 :initform nil))
   (:name-slot url))

  ((bzr "hudson.plugins.bazaar.BazaarSCM")
   ((url :type  string
         :xpath "source/text()"))
   (:name-slot url))

  ((mercurial "hudson.plugins.mercurial.MercurialSCM" :plugin "mercurial@1.54")
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
