;;;; api.jenkins.management.asd ---
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:jenkins.management-system
  (:use
   #:cl
   #:asdf))

(cl:in-package #:jenkins.management-system)

(defsystem :jenkins.management
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     "0.1.0" #+no (version/string)
  :license     "LLGPLv3; see COPYING file for details."
  :description "DESCRIPTION"
  :depends-on  (:alexandria
		:let-plus
		:more-conditions
		:cl-interpol
		:lparallel

		:inferior-shell

		(:version :jenkins.api "0.1.0"))
  :components  ((:module     "management"
		 :pathname   "src/management"
		 :serial     t
		 :components ((:file       "package")
			      (:file       "conditions")
			      (:file       "variables")
			      (:file       "run-nodes")
			      (:file       "setup")))))
