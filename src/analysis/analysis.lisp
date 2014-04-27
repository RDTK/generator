;;;; analysis.lisp ---
;;;;
;;;; Copyright (C) 2012, 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.analysis)

(defmethod project-kind ((directory pathname))
  (cond
    ((directory (merge-pathnames "*.asd" directory) )
     :asdf)

    ((probe-file (merge-pathnames "setup.py" directory) )
     :setuptools)

    ((probe-file (merge-pathnames "pom.xml" directory) )
     :maven)

    #+no ((probe-file (merge-pathnames "build.xml" directory) )
     :ant)

    ((and (probe-file (merge-pathnames "CMakeLists.txt" directory))
          (directory (merge-pathnames "c*/*ebian*.cmake" directory)))
     :cmake/debian)

    ((probe-file (merge-pathnames "CMakeLists.txt" directory))
     :cmake)

    (t
     :freestyle)))

(defmethod analyze ((source puri:uri) (kind (eql :auto))
                    &rest args
                    &key
                    scm
                    (branches       (puri:uri-fragment source))
                    (temp-directory #P"/tmp/"))
  (setf (puri:uri-fragment source) nil)

  (let+ (((&accessors-r/o (scheme puri:uri-scheme)) source)
         (source/string (princ-to-string source))
         (scheme (case scheme
                   ((:git :svn) scheme)
                   (t           (cond
                                  (scm
                                   (make-keyword (string-upcase scm)))
                                  ((search "git" source/string :test #'char-equal) :git)
                                  ((search "svn" source/string :test #'char-equal) :svn)
                                  (t (error "~@<Cannot handle URI ~A.~@:>"
                                            source))))))
         (temp-directory (default-temporary-directory
                          :base temp-directory :hint "project")))
    (apply #'analyze source scheme
           :branches       branches
           :temp-directory temp-directory
           (remove-from-plist args :branches :temp-directory))))

(defmethod analyze ((source pathname) (kind (eql :auto))
                    &key)
  (let ((kind (or (project-kind source)
                  (error "~@<Could not automatically determine the ~
                          project kind of ~S.~@:>"
                         source))))
    (analyze source kind)))

(defmethod analyze ((source pathname) (kind (eql :freestyle))
                    &key)
  '())
