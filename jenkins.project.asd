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

(let* ((version-file (merge-pathnames "version.sexp" *load-truename*))
       stream)
  (when (probe-file version-file)
    (setf stream (open version-file)))

  (defparameter +version-revision+ (if stream (read stream) 0)
    "Revision component of version number.")

  (defparameter +version-commit+ (when stream (read stream))
    "Commit component of version number.")

  (when stream (close stream)))

(defun version/list (&key
                     (revision? t)
                     commit?)
  "Return a version of the form (MAJOR MINOR [REVISION [COMMIT]])
   where REVISION and COMMIT are optional.

   REVISION? controls whether REVISION should be included. Default
   behavior is to include REVISION.

   COMMIT? controls whether COMMIT should be included. Default
   behavior is to not include COMMIT."
  (append (list +version-major+ +version-minor+)
          (when revision? (list +version-revision+))
          (when (and commit? +version-commit+)
            (list +version-commit+))))

(defun version/string (&rest args
                       &key
                       revision?
                       commit?)
  "Return a version string of the form
   \"MAJOR.MINOR[.REVISION[-.COMMIT]]\" where REVISION and COMMIT are
   optional.

   See `version/list' for details on keyword parameters."
  (declare (ignore revision? commit?))
  (format nil "~{~A.~A~^.~A~^-~A~}" (apply #'version/list args)))

;;; System definition

(defsystem :jenkins.project
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "LLGPLv3; see COPYING file for details."
  :description "TODO"
  :depends-on  (:alexandria
                (:version :split-sequence        "1.1")
                :iterate
                (:version :let-plus              "0.1")
                (:version :more-conditions       "0.1.0")
                (:version :utilities.print-items "0.1.0")
                :lparallel
                :log4cl

                :puri
                :xml.location
                :cl-interpol
                (:version :esrap                 "0.9")
                (:version :cl-ppcre              "2.0.3")
                (:version :cl-json               "0.4.1")
                :inferior-shell

                (:version :rosetta               "0.2")

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
