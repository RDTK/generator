;;;; job-builder.lisp --- Model classes for builder implementations.
;;;;
;;;; Copyright (C) 2012-2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.api)

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
              (cond ((string= class ,default-provider)
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
   ((project-name :type      string
                  :xpath     (:version
                              ("copyartifact@1.25" "projectName/text()")
                              (t                   "project/text()")))
    (filter       :type      (list/comma string)
                  :optional? t)
    (target       :type      string)
    (flatten?     :type      boolean
                  :xpath     "flatten/text()")
    ;; TODO(jmoringe, 2012-12-13): temp
    (clazz        :type      string
                  :xpath     "selector/@class"))
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
