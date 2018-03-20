;;;; jenkins.project.commandline-options.asd --- System definition for jenkins.project.commandline-options.
;;;;
;;;; Copyright (C) 2017, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(defsystem "jenkins.project.commandline-options"
  :description "Commandline options of the generator program."
  :license     "GPLv3" ; see COPYING file for details.

  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :version     (:read-file-form "version-string.sexp")
  :depends-on  ("alexandria"
                "split-sequence"
                (:version "let-plus"              "0.2")
                (:version "more-conditions"       "0.2")

                (:version "configuration.options" "0.6"))

  :components  ((:module     "commandline-options"
                 :pathname   "src/commandline-options"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "types")
                              (:file       "conditions")
                              (:file       "protocol")
                              (:file       "options")
                              (:file       "macros")
                              (:file       "help")))))
