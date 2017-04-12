;;;; jenkins.api.asd --- System definition for the jenkins.api system.
;;;;
;;;; Copyright (C) 2011-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:jenkins.api-system
  (:use
   #:cl
   #:asdf)

  (:export
   #:version/list
   #:version/string))

(cl:in-package #:jenkins.api-system)

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

(defsystem :jenkins.api
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "LLGPLv3; see COPYING file for details."
  :description "Bindings for Jenkins' REST API."
  :depends-on  (:alexandria #+maybe-later (:version :alexandria      "0.0.0")
                (:version :split-sequence  "1.1")
                (:version :closer-mop      "0.61")
                :iterate #+maybe-later (:version :iterate         "1.4.4")
                (:version :let-plus        "0.1")
                (:version :more-conditions "0.1.0")

                (:version :cl-ppcre        "2.0.3")
                :puri
                (:version :drakma          "1.2.8")
                (:version :xml.location    "0.2.0")
                (:version :cl-json         "0.4.1"))
  :components  ((:module     "model"
                 :pathname   "src/api/model"
                 :depends-on ("api")
                 :serial     t
                 :components ((:file     "view")))

                (:module     "api"
                 :pathname   "src/api"
                 :serial     t
                 :components ((:file     "package")
                              (:file     "types")
                              (:file     "conditions")
                              (:file     "variables")
                              (:file     "protocol")
                              (:file     "conversion")
                              (:file     "classes")
                              (:file     "api")))

                (:module     "dsl"
                 :pathname   "src/dsl"
                 :depends-on ("api")
                 :serial     t
                 :components ((:file     "package")
                              (:file     "macros")))))
