;;;; jenkins.project.more-conditions-patch.asd --- Patch for more-conditions system.
;;;;
;;;; Copyright (C) 2017, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(defsystem "jenkins.project.more-conditions-patch"
  :description "Patch more-conditions system."
  :license     "GPLv3" ; see COPYING file for details.

  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :version     (:read-file-form "version-string.sexp")
  :depends-on  ((:version "more-conditions" "0.2"))

  :components  ((:file       "more-conditions-patch"
                 :pathname   "src/more-conditions-patch")))
