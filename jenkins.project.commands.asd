;;;; jenkins.project.commands.asd --- System definition for generator commands.
;;;;
;;;; Copyright (C) 2016, 2017, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(defsystem "jenkins.project.commands"
  :description "Command for the generator program."
  :license     "GPLv3" ; see COPYING file for details.

  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :version     (:read-file-form "version-string.sexp")
  :depends-on  ("alexandria"
                (:version "let-plus"                                   "0.2")
                (:version "more-conditions"                            "0.2")

                (:version "configuration.options"                      "0.6")
                (:version "configuration.options-and-service-provider" "0.6")
                (:version "configuration.options-and-puri"             "0.6")

                (:version "jenkins.project"                            (:read-file-form "version-string.sexp"))
                (:version "jenkins.project.commandline-options"        (:read-file-form "version-string.sexp")))

  :components  ((:module     "commands"
                 :pathname   "src/commands"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "conditions")
                              (:file       "protocol")

                              (:file       "phases")

                              (:file       "value-types")
                              (:file       "mixins")

                              (:file       "functions-version")
                              (:file       "functions-input")
                              (:file       "functions-check")
                              (:file       "functions-deploy")

                              (:file       "command-version")
                              (:file       "command-help")

                              (:file       "command-info-variables")
                              (:file       "command-info-aspects")
                              (:file       "command-generate")

                              (:file       "command-analyze")

                              (:file       "command-validate")
                              (:file       "command-platform-requirements")
                              (:file       "command-report")))))
