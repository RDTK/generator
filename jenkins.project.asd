;;;; jenkins.project.asd ---
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:jenkins.project-system
  (:use
   #:cl
   #:asdf)

  (:export
   #:version/list
   #:version/string))

(cl:in-package #:jenkins.project-system)

;;; Version stuff

(defparameter +version-major+ 0
  "Major component of version number.")

(defparameter +version-minor+ 1
  "Minor component of version number.")

(defparameter +version-revision+ 0
  "Revision component of version number.")

(defun version/list ()
  "Return a version of the form (MAJOR MINOR REVISION)."
  (list +version-major+ +version-minor+ +version-revision+))

(defun version/string ()
  "Return a version string of the form \"MAJOR.MINOR.REVISION\"."
  (format nil "~{~A.~A.~A~}" (version/list)))

;;; System definition

(defsystem :jenkins.project
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "LLGPLv3; see COPYING file for details."
  :description "TODO"
  :depends-on  (:alexandria #+maybe-later (:version :alexandria      "0.0.0")
                (:version :split-sequence        "1.1")
                :iterate #+maybe-later (:version :iterate         "1.4.4")
                (:version :let-plus              "0.1")
                (:version :more-conditions       "0.1.0")
                (:version :utilities.print-items "0.1.0")
                :log4cl

                :puri
                :xml.location
                :cl-interpol
                (:version :esrap                 "0.9")
                (:version :cl-ppcre              "2.0.3")
                (:version :cl-json               "0.4.1")
                :inferior-shell #+no (:version :inferior-shell  )
                :rosetta ;; TODO temp

                (:version :jenkins.api           "0.1"))
  :components  ((:module     "analysis"
                 :pathname   "src/analysis"
                 :serial     t
                 :components ((:file     "package")
                              (:file     "util")
                              (:file     "protocol")
                              (:file     "analysis")
                              (:file     "version")

                              ;; Version control systems
                              (:file     "git")
                              (:file     "subversion")

                              ;; Build systems
                              (:file     "license")
                              (:file     "pkg-config")
                              (:file     "cmake")
                              (:file     "asdf")
                              (:file     "maven")
                              (:file     "setuptools")))

                (:module     "project"
                 :pathname   "src/project"
                 :depends-on ("analysis")
                 :serial     t
                 :components ((:file     "package")
                              (:file     "conditions")
                              (:file     "protocol")
                              (:file     "util")
                              (:file     "variables")

                              (:file     "mixins")
                              (:file     "aspect")
                              (:file     "aspects")
                              (:file     "classes-spec")
                              (:file     "classes-model")

                              (:file     "json")))))
