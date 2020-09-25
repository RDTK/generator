;;;; build-generator.commands.asd --- System definition for generator commands.
;;;;
;;;; Copyright (C) 2016, 2017, 2018, 2019, 2020 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(defsystem "build-generator.commands"
  :description "Command for the generator program."
  :license     "GPLv3" ; see COPYING file for details.

  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :version     (:read-file-form "version-string.sexp")
  :depends-on  ("alexandria"
                (:version "let-plus"                                   "0.2")
                (:version "optima"                                     "1.0")
                (:version "more-conditions"                            "0.2")

                (:version "configuration.options"                      "0.10")
                (:version "configuration.options-and-service-provider" "0.10")
                (:version "configuration.options-and-puri"             "0.10")

                (:version "cl-ppcre"                                   "2.0.11") ; for bcrypt
                (:version "nibbles"                                    "0.13")   ; for bcrypt
                "zip"

                (:version "build-generator"                            (:read-file-form "version-string.sexp"))
                (:version "build-generator.commandline-options"        (:read-file-form "version-string.sexp")))

  :components  ((:module     "bcrypt"
                 :pathname   "src/bcrypt"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "base64")
                              (:file       "bcrypt")))

                (:module     "resources"
                 :pathname   "src/resources"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "protocol")
                              (:file       "conditions")
                              (:file       "resources")))

                (:module     "steps"
                 :pathname   "src/steps"
                 :serial     t
                 :depends-on ("bcrypt" "resources")
                 :components ((:file       "package")

                              (:file       "protocol")
                              (:file       "macros")

                              (:file       "jenkins-install")))

                (:module     "commands"
                 :pathname   "src/commands"
                 :serial     t
                 :depends-on ("steps")
                 :components ((:file       "package")
                              (:file       "conditions")
                              (:file       "protocol")

                              (:file       "util")
                              (:file       "phases")

                              (:file       "value-types")
                              (:file       "mixins")

                              (:file       "functions-version")
                              (:file       "functions-input")
                              (:file       "functions-check")

                              (:file       "command-version")
                              (:file       "command-help")
                              (:file       "command-config")

                              (:file       "command-info-variables")
                              (:file       "command-info-aspects")
                              (:file       "command-generate")

                              (:file       "command-analyze")

                              (:file       "command-validate")
                              (:file       "command-platform-requirements")
                              (:file       "command-report")

                              (:file       "command-install-jenkins")
                              (:file       "command-create-jenkins-user")))))
