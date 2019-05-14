;;;; job-build-wrapper.lisp --- Model classes for builder-wrappre implementations.
;;;;
;;;; Copyright (C) 2012-2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.api)

(define-interface-implementations (build-wrapper)
  ;; We use the EnvInjectJobProperty instead
  #+disabled ((environment "EnvInjectBuildWrapper" :plugin "envinject@2.1.6")
   ((entries :type     (equals+newline/plist keyword string)
             :xpath    "info/propertiesContent/text()"
             :initform '()))
   (:name-slot nil))

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
   (:name-slot kind))

  ((sonar "hudson.plugins.sonar.SonarBuildWrapper"
          :plugin "sonar@2.6.1")
   ())

  ((timestamper "hudson.plugins.timestamper.TimestamperBuildWrapper"
                :plugin "timestamper@1.9")
   ()))


