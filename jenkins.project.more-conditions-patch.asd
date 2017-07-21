;;;; jenkins.project.more-conditions-patch.asd --- Patch for more-conditions system.
;;;;
;;;; Copyright (C) 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:jenkins.project.more-conditions-patch-system
  (:use
   #:cl
   #:asdf))

(cl:in-package #:jenkins.project.more-conditions-patch-system)

#.(progn
    (load (merge-pathnames "jenkins.project.asd" *load-truename*))
    (values))

(defsystem :jenkins.project.more-conditions-patch
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(jenkins.project-system:version/string)
  :license     "GPLv3" ; see COPYING file for details.
  :description "Patch more-conditions system."
  :depends-on  ((:version :more-conditions "0.2"))
  :components  ((:file       "more-conditions-patch"
                 :pathname   "src/more-conditions-patch")))
