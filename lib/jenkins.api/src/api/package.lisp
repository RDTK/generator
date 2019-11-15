;;;; package.lisp --- Package definition for api module.
;;;;
;;;; Copyright (C) 2012-2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:jenkins.api
  (:use
   #:cl
   #:alexandria
   #:split-sequence
   #:iterate
   #:let-plus
   #:more-conditions)

  ;; Types
  (:export
   #:job-name-character?
   #:job-name?           #:job-name)

  ;; Variables
  (:export
   #:*base-url*
   #:*username*
   #:*password*)

  ;; API
  (:export
   #:id
   #:commit!)

  ;; Node
  (:export
   #:node

   #:node
   #:all-nodes
   #:node-config
   #:make-node
   #:delete-node

   #:name
   #:description
   #:host
   #:mode
   #:label
   #:environment

   #:online?
   #:mark-online!
   #:mark-offline!)

  ;; Job
  (:export
   #:job

   #:job ; TODO find-job?
   #:all-jobs ; TODO jobs?
   #:job-config
   #:job?
   #:make-job
   #:delete-job

   #:kind                           ; matrix or regular; setfable
   #:description
   #:upstream
   #:children
   #:block-on-downstream-build?
   #:block-on-upstream-build?
   #:can-roam?
   #:restrict-to-slaves
   #:disabled?

   #:properties     #:properties-of-type
   #:triggers       #:triggers-of-type ; Interface-based children
   #:repository                     ; Note: only one repository
   #:build-wrappers #:build-wrappers-of-type
   #:builders       #:builders-of-type
   #:publishers     #:publishers-of-type

   #:slaves                         ; Not sure about these
   #:environment
   #:permissions

   #:copy-job/fixup
   #:build!
   #:enable!
   #:disable!

   #:relate                         ; Up/downstream relations
   #:unrelate)

  ;; SCM interface
  (:export
   #:scm/git
   #:url
   #:credentials
   #:branches
   #:local-branch
   #:clone-timeout
   #:wipe-out-workspace?
   #:checkout-submodules?
   #:internal-tag?
   #:browser-kind
   #:browser-url

   #:scm/svn
   #:url
   #:local-directory
   #:checkout-strategy

   #:scm/bzr
   #:url

   #:scm/mercurial
   #:url
   #:credentials
   #:revision-type
   #:branch
   #:sub-directory
   #:clean?
   #:modules

   #:scm/null)

  ;; Properties interface
  (:export
   #:property/discard-builds
   #:keep-builds/days
   #:keep-builds/count
   #:keep-artifacts/days
   #:keep-artifacts/count

   #:property/parameters
   #:parameters

   #:property/github
   #:project-url
   #:display-name

   #:property/docker
   #:image
   #:force-pull?

   #:property/redmine
   #:instance
   #:project-name)

  ;; Trigger interface
  (:export
   #:trigger/scm
   #:spec
   #:ignore-post-commit-hooks?

   #:trigger/timer
   #:spec

   #:trigger/github
   #:spec)

  ;; Build-wrapper interface
  (:export
   #:build-wrapper/timeout
   #:kind
   #:timeout/minutes
   #:faild-build?

   #:build-wrapper/sonar

   #:build-wrapper/timestamper

   #:build-wrapper/ansi-color
   #:color-map)

  ;; Builder interface
  (:export
   #:builder/shell
   #:command

   #:builder/batch
   #:command

   #:builder/cmake
   #:command

   #:builder/ant
   #:targets
   #:properties

   #:builder/maven
   #:targets
   #:properties
   #:private-repository?

   #:builder/copy-artifact
   #:project-name
   #:filter
   #:target
   #:flatten?

   #:builder/groovy
   #:builder/system-groovy
   #:code
   #:sandbox?)

  ;; Publisher interface
  (:export
   #:publisher/ssh
   #:target
   #:remote-directory

   #:publisher/warnings
   #:file-parsers
   #:console-parsers

   #:warning-parser/file
   #:pattern
   #:name

   #:warning-parser/console
   #:name

   #:publisher/checkstyle
   #:pattern

   #:publisher/pmd
   #:pattern

   #:publisher/tasks
   #:threshold-limit
   #:keywords/low
   #:keywords/normal
   #:keywords/high

   #:publisher/archive-artifacts
   #:files
   #:only-latest?

   #:publisher/fingerprint
   #:targets
   #:build-artifacts?

   #:publisher/cobertura
   #:report-file

   #:publisher/html
   #:reports

   #:html-report
   #:name
   #:base-directory
   #:include
   #:index-files
   #:keep-all?
   #:allow-missing?

   #:publisher/email-notification
   #:recipients

   #:publisher/xunit
   #:types
   #:xunit/type

   #:publisher/junit
   #:pattern
   #:keep-long-stdio?
   #:health-scale-factor
   #:allow-empty-results?

   #:publisher/issues-recorder
   #:analysis-tools

   #:analysis-tool/open-tasks

   #:analysis-tool/gcc4

   #:analysis-tool/java

   #:analysis-tool/maven

   #:analysis-tool/checkstyle

   #:analysis-tool/pmd

   #:analysis-tool/groovy
   #:parser)

  ;; Build
  (:export
   #:build

   #:build
   #:all-builds
   #:last-builds
   #:build-config
   #:make-build
   #:delete-build

   #:job
   #:slave
   #:slave-name
   #:result
   #:building?
   #:failed?)

  ;; View
  (:export
   #:view

   #:view
   #:all-views
   #:view-config
   #:make-view
   #:delete-view

   #:jobs)

  (:documentation
   "Model classes for Jenkins configuration and functions for using the REST API."))
