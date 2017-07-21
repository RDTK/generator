;;;; jenkins.project.commandline-interface.asd --- System definition for generator binary.
;;;;
;;;; Copyright (C) 2013, 2014, 2015, 2016, 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:jenkins.project.commandline-interface-system
  (:use
   #:cl
   #:asdf))

(cl:in-package #:jenkins.project.commandline-interface-system)

#.(progn
    (load (merge-pathnames "jenkins.project.asd" *load-truename*))
    (values))

(defsystem :jenkins.project.commandline-interface
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(jenkins.project-system:version/string)
  :license     "GPLv3" ; see COPYING file for details.
  :description "Commandline interface of the generator program."
  :depends-on  (:alexandria
                (:version :let-plus                              "0.2")
                :iterate
                (:version :more-conditions                       "0.2")

                :net.didierverna.clon
                (:version :configuration.options                 "0.1")
                (:version :configuration.options-syntax-ini      "0.1")

                (:version :jenkins.project                       #.(jenkins.project-system:version/string))
                (:version :jenkins.project.more-conditions-patch #.(jenkins.project-system:version/string)))
  :components  ((:module     "commandline-interface"
                 :pathname   "src/commandline-interface"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "conditions")
                              (:file       "configuration")
                              (:file       "main"))))
  :entry-point "JENKINS.PROJECT.COMMANDLINE-INTERFACE:MAIN")
