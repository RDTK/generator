;;;; jenkins.api.asd --- System definition for the jenkins.api system.
;;;;
;;;; Copyright (C) 2011-2022 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(defsystem "jenkins.api"
  :description "Bindings for Jenkins' REST API."
  :license     "GPLv3" ; see COPYING file for details.

  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :version     "0.1.0"
  :depends-on  ("alexandria"
                (:version "split-sequence"        "1.1")
                (:version "closer-mop"            "0.61")
                (:version "let-plus"              "0.1")
                (:version "more-conditions"       "0.1.0")
                (:version "utilities.print-items" "0.3")
                (:version "log4cl"                "1.1.3")

                (:version "cl-ppcre"              "2.0.3")
                "puri"
                (:version "drakma"                "1.2.8")
                (:version "xml.location"          "0.2.0")
                (:version "cl-json"               "0.4.1"))

  :components  ((:module     "api-early"
                 :pathname   "src/api"
                 :serial     t
                 :components ((:file     "package")
                              (:file     "types")
                              (:file     "conditions")
                              (:file     "util")
                              (:file     "protocol")
                              (:file     "conversion")))

                (:module     "model"
                 :pathname   "src/api/model"
                 :depends-on ("api-early")
                 :serial     t
                 :components ((:file     "model-class")
                              (:file     "interface")

                              (:file     "build")

                              (:file     "job-scm")
                              (:file     "job-trigger")
                              (:file     "job-builder")
                              (:file     "job-build-wrapper")
                              (:file     "job-property")
                              (:file     "job-publisher")
                              (:file     "job")

                              (:file     "view")))

                (:module     "api-late"
                 :pathname   "src/api"
                 :depends-on ("api-early" "model")
                 :serial     t
                 :components ((:file     "classes")

                              (:file     "http")
                              (:file     "csrf")
                              (:file     "api")))))
