;;;; jenkins.api.asd --- System definition for the jenkins.api system.
;;;;
;;;; Copyright (C) 2011-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(defsystem "jenkins.api"
  :description "Bindings for Jenkins' REST API."
  :license     "LLGPLv3" ; see COPYING file for details.

  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :version     "0.1.0"
  :depends-on  ("alexandria"
                (:version "split-sequence"  "1.1")
                (:version "closer-mop"      "0.61")
                "iterate"
                (:version "let-plus"        "0.1")
                (:version "more-conditions" "0.1.0")
                (:version "log4cl"          "1.1.3")

                (:version "cl-ppcre"        "2.0.3")
                "puri"
                (:version "drakma"          "1.2.8")
                (:version "xml.location"    "0.2.0")
                (:version "cl-json"         "0.4.1"))

  :components  ((:module     "model"
                 :pathname   "src/api/model"
                 :depends-on ("api")
                 :serial     t
                 :components ((:file     "view")))

                (:module     "api"
                 :pathname   "src/api"
                 :serial     t
                 :components ((:file     "package")
                              (:file     "types")
                              (:file     "conditions")
                              (:file     "variables")
                              (:file     "protocol")
                              (:file     "conversion")
                              (:file     "classes")
                              (:file     "csrf")
                              (:file     "api")))

                (:module     "dsl"
                 :pathname   "src/dsl"
                 :depends-on ("api")
                 :serial     t
                 :components ((:file     "package")
                              (:file     "macros")))))
