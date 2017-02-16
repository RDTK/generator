;;;; jenkins.project.asd ---
;;;;
;;;; Copyright (C) 2011-2017 Jan Moringen
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

(defparameter +version-minor+ 12
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
  :license     "LLGPLv3" ; see COPYING file for details.
  :description "TODO"
  :depends-on  (:alexandria
                (:version :split-sequence        "1.1")
                :iterate
                (:version :let-plus              "0.1")
                (:version :more-conditions       "0.1.0")
                (:version :utilities.print-items "0.1.0")
                (:version :utilities.print-tree  "0.1.0")
                :lparallel
                :log4cl

                :puri
                :xml.location
                :cl-interpol
                (:version :esrap                 "0.9")
                (:version :cl-ppcre              "2.0.3")
                (:version :cl-json               "0.4.1")
                :cl-store
                :inferior-shell
                :ironclad

                (:version :rosetta               "0.2")

                (:version :jenkins.api           "0.1")

                :cl-dot)
  :components  ((:file       "cxml-hack"
                 :pathname   "src/cxml-patch")

                (:module     "version"
                 :pathname   "src/version"
                 :serial     t
                 :components ((:file     "package")
                              (:file     "version")))

                (:module     "analysis"
                 :pathname   "src/analysis"
                 :depends-on ("version")
                 :serial     t
                 :components ((:file     "package")
                              (:file     "util")
                              (:file     "conditions")
                              (:file     "protocol")
                              (:file     "analysis")

                              ;; Version control systems
                              (:file     "scm-null")
                              (:file     "archive")
                              (:file     "git")
                              (:file     "subversion")
                              (:file     "mercurial")

                              ;; Build systems
                              (:file     "license")
                              (:file     "pkg-config")
                              (:file     "cmake")
                              (:file     "asdf")
                              (:file     "maven")
                              (:file     "ant")
                              (:file     "setuptools")
                              (:file     "ros-package")
                              (:file     "ros-packages")

                              ;; Platform analysis
                              (:file     "platform")))

                (:module     "model"
                 :pathname   "src/model"
                 :depends-on ("model-variables")
                 :serial     t
                 :components ((:file     "package")
                              (:file     "conditions")
                              (:file     "protocol")
                              (:file     "util")

                              (:file     "mixins")))

                (:module     "model-variables"
                 :pathname   "src/model/variables"
                 :serial     t
                 :components ((:file     "package")
                              (:file     "conditions")
                              (:file     "variables")
                              (:file     "protocol")
                              (:file     "schema")
                              (:file     "trace")
                              (:file     "model")
                              (:file     "grammar")
                              (:file     "evaluation")

                              (:file     "mixins")))

                (:module     "model-project"
                 :pathname   "src/model/project"
                 :depends-on ("version" "analysis" "model" "model-variables" "model-aspects") ; TODO
                 :serial     t
                 :components ((:file     "package")
                              (:file     "variables")
                              (:file     "protocol")
                              (:file     "classes-spec")
                              (:file     "classes-model")

                              (:file     "json")

                              (:file     "progress")))

                (:module     "model-aspects"
                 :pathname   "src/model/aspects"
                 :depends-on ("model")
                 :serial     t
                 :components ((:file     "package")
                              (:file     "protocol")

                              (:file     "aspect")
                              (:file     "mixins")
                              (:file     "aspects")))

                (:module     "report"
                 :pathname   "src/report"
                 :depends-on ("model-project")
                 :serial     t
                 :components ((:file     "package")
                              (:file     "protocol")
                              (:file     "util")
                              (:file     "json")
                              (:file     "graphviz")))))
