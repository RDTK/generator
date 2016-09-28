;;;; jenkins.project.commandline-interface.asd --- System definition for generator binary.
;;;;
;;;; Copyright (C) 2013, 2014, 2015, 2016, 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(defsystem :jenkins.project.commandline-interface
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     (:read-file-form "version-string.sexp")
  :license     "GPLv3" ; see COPYING file for details.
  :description "Commandline interface of the generator program."
  :depends-on  (:alexandria
                (:version :let-plus                              "0.2")
                :iterate
                (:version :more-conditions                       "0.2")

                :net.didierverna.clon
                (:version :configuration.options                 "0.1")
                (:version :configuration.options-syntax-ini      "0.1")

                (:version :jenkins.project                       (:read-file-form "version-string.sexp"))

                :project-automation.commands

                (:version :jenkins.project.more-conditions-patch (:read-file-form "version-string.sexp")))
  :components  ((:module     "commandline-interface"
                 :pathname   "src/commandline-interface"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "conditions")
                              (:file       "configuration")
                              (:file       "main"))))
  :entry-point "JENKINS.PROJECT.COMMANDLINE-INTERFACE:MAIN")
