;;;; build-generator.commandline-interface.asd --- System definition for generator binary.
;;;;
;;;; Copyright (C) 2013-2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(defsystem "build-generator.commandline-interface"
  :description "Commandline interface of the generator program."
  :license     "GPLv3" ; see COPYING file for details.

  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :version     (:read-file-form "version-string.sexp")
  :depends-on  ("alexandria"
                (:version "let-plus"                              "0.2")
                (:version "more-conditions"                       "0.2")

                (:version "configuration.options"                 "0.10")
                (:version "configuration.options-syntax-ini"      "0.10")
                (:version "configuration.options-and-puri"        "0.10")

                (:version "build-generator"                       (:read-file-form "version-string.sexp"))
                (:version "build-generator.more-conditions-patch" (:read-file-form "version-string.sexp"))
                (:version "build-generator.commands"              (:read-file-form "version-string.sexp")))

  :components  ((:module     "commandline-interface"
                 :pathname   "src/commandline-interface"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "value-types")
                              (:file       "configuration")
                              (:file       "main"))))

  :build-operation program-op
  :build-pathname  "build-generator"
  :entry-point     "BUILD-GENERATOR.COMMANDLINE-INTERFACE:MAIN")
