;;;; job-property.lisp --- Model classes for property implementations.
;;;;
;;;; Copyright (C) 2012-2021 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.api)

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
                   ("hudson.model.TextParameterDefinition"     :text)
                   ("hudson.model.StringParameterDefinition"   :string)
                   ("hudson.model.BooleanParameterDefinition"  :boolean)
                   ("hudson.model.PasswordParameterDefinition" :password)
                   (t                                          class))
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
      (setf class        (case kind
                           (:text     "hudson.model.TextParameterDefinition")
                           (:string   "hudson.model.StringParameterDefinition")
                           (:boolean  "hudson.model.BooleanParameterDefinition")
                           (:password "hudson.model.PasswordParameterDefinition")
                           (t         kind))
            name1        name
            description1 (or description "")
            default1     (or default ""))))
  dest)

(define-interface-implementations (property :plural-name properties)
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
                  :initform nil))
   (:name-slot display-name))

  ((docker "it.dockins.dockerslaves.spec.ContainerSetDefinition"
           :plugin "docker-slaves@1.0.5")
   ((image       :type string
                 :xpath "buildHostImage[@class=\"it.dockins.dockerslaves.spec.ImageIdContainerDefinition\"]/image/text()")
    (force-pull? :type boolean
                 :xpath "buildHostImage[@class=\"it.dockins.dockerslaves.spec.ImageIdContainerDefinition\"]/forcePull/text()"))
   (:name-slot image))

  ((redmine "hudson.plugins.redmine.RedmineProjectProperty"
            :plugin "redmine@0.21")
   ((instance     :type     string
                  :xpath    "redmineWebsiteName/text()")
    (project-name :type     string
                  :xpath    "projectName/text()"))
   (:name-slot project-name)))
