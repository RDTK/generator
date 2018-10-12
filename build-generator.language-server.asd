;;;; build-generator.language-server.asd --- LSP implementation for the generator language.
;;;;
;;;; Copyright (C) 2017, 2018, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(defsystem "build-generator.language-server"
  :description "Language server for the generator recipe language."
  :license     "GPLv3" ; see COPYING file for details.

  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :version     (:read-file-form "version-string.sexp")
  :depends-on  ("alexandria"
                "split-sequence"
                (:version "let-plus"                             "0.2")

                "swank" ; TODO temp

                (:version "text.source-location.lookup"          "0.1")

                (:version "protocol.language-server"             "0.1")
                (:version "protocol.language-server.contributor" "0.1")

                (:version "build-generator"                      (:read-file-form "version-string.sexp")))

  :components  ((:module     "language-server"
                 :pathname   "src/language-server"
                 :serial     t
                 :components ((:file       "package")

                              (:file       "workspace")
                              (:file       "document")

                              (:file       "diagnostics")
                              (:file       "context")
                              (:file       "hover")
                              (:file       "completion")

                              ; TODO later (:file       "parser")
                              ))))
