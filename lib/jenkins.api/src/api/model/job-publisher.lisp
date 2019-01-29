;;;; job-publisher.lisp --- Model classes for publisher implementations .
;;;;
;;;; Copyright (C) 2012-2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.api)

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
