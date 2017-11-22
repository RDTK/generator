;;;; jenkins.project.language-server.asd --- System definition for generator binary.
;;;;
;;;; Copyright (C) 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(defsystem :jenkins.project.language-server
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     (:read-file-form "version-string.sexp")
  :license     "GPLv3" ; see COPYING file for details.
  :description "Language server for the generator recipe language."
  :depends-on  (:alexandria
                :split-sequence
                (:version :let-plus                 "0.2")

                :swank

                (:version :jenkins.project          (:read-file-form "version-string.sexp"))
                (:version :jenkins.project.commands (:read-file-form "version-string.sexp")))
  :components  ((:module     "language-server"
                 :pathname   "src/language-server"
                 :serial     t
                 :components ((:file       "package")

                              (:file       "protocol")

                              (:file       "transport")
                              (:file       "connection")
                              (:file       "messages")

                              (:file       "context")
                              (:file       "context-methods")

                              (:file       "workspace")
                              (:file       "workspace-methods")

                              (:file       "document")
                              (:file       "document-methods")

                              (:file       "language-server")))

                (:file       "command-language-server"
                 :depends-on ("language-server")
                 :pathname   "src/commands/command-language-server")))
