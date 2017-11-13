;;;; analysis.lisp ---
;;;;
;;;; Copyright (C) 2012-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.analysis)

(defun guess-project-natures (directory)
  (or (append
       (when (or (directory (merge-pathnames "configure" directory))
                 (directory (merge-pathnames "configure.in" directory))
                 (directory (merge-pathnames "configure.ac" directory)))
         '(:autotools))

       (when (directory (merge-pathnames "*.asd" directory))
         '(:asdf))

       (when (probe-file (merge-pathnames "setup.py" directory))
         '(:setuptools))

       (when (probe-file (merge-pathnames "pom.xml" directory))
         '(:maven))

       (when (probe-file (merge-pathnames "build.xml" directory))
         '(:ant))

       (cond
         ((probe-file (merge-pathnames "package.xml" directory))
          '(:ros-package))
         ((directory (merge-pathnames "*/package.xml" directory))
          '(:ros-packages)))

       (when (probe-file (merge-pathnames "CMakeLists.txt" directory))
         '(:cmake)))

      '(:freestyle)))

(defun guess-scm (url scm)
  (let+ (((&accessors-r/o (scheme puri:uri-scheme)) url)
         (source/string nil)
         ((&flet source/string ()
            (or source/string (setf source/string (princ-to-string url)))))
         ((&flet source-contains (substring)
            (search substring (source/string) :test #'char-equal))))
    (cond
      (scm                          (make-keyword (string-upcase scm)))
      ((member scheme '(:git :svn)) scheme)
      ((source-contains "git")      :git)
      ((source-contains "svn")      :svn)
      ((source-contains "hg")       :mercurial)
      (t                            (error "~@<Cannot handle URI ~A.~@:>"
                                           url)))))

(defmethod analyze ((source puri:uri) (kind (eql :auto))
                    &rest args
                    &key
                    scm
                    (versions       `((:branch . ,(puri:uri-fragment source)))) ; TODO
                    (temp-directory #P"/tmp/"))
  (setf (puri:uri-fragment source) nil)

  (let ((kind           (guess-scm source scm))
        (temp-directory (default-temporary-directory
                            :base temp-directory :hint "project")))
    (apply #'analyze source kind
           :versions       versions
           :temp-directory temp-directory
           (remove-from-plist args :versions :temp-directory))))

(defmethod analyze ((source pathname) (kind (eql :auto))
                    &key
                    (natures nil natures-supplied?)
                    branches)
  (let ((natures (if natures-supplied?
                     natures
                     (guess-project-natures source))))
    (log:info "~@<Analyzing ~A ~:[without natures~:;with nature~*~:P ~
               ~2:*~{~A~^, ~}~]~@:>"
              source natures (length natures))
    (cond
      ((emptyp natures)
       (mapcar (lambda (branch) (cons branch '())) branches))
      ((length= 1 natures)
       (analyze source (first natures)))
      (t
       (let ((merged (make-hash-table)))
         (dolist (result (analyze source natures))
           (loop :for (key value) :on result :by #'cddr :do
                    (cond
                      ((not (nth-value 1 (gethash key merged)))
                       (setf (gethash key merged) value))
                      ((listp (gethash key merged))
                       (appendf (gethash key merged) (ensure-list value))))))
         (hash-table-plist merged))))))

(defmethod analyze :around ((source pathname) (kind (eql :auto)) &key)
  (let ((result (call-next-method)))
    (cond
      ((getf result :license)
       result)
      ((when-let ((license (analyze source :license)))
         (list* :license license result)))
      (t
       result))))

(defmethod analyze ((source pathname) (kind cons) &rest args &key)
  (mapcan (lambda (kind)
            (with-simple-restart (continue "~@<Do not analyze ~A with ~
                                            nature ~A~@:>"
                                           source kind)
              (list (apply #'analyze source kind args))))
          kind))

(defmethod analyze ((source pathname) (kind (eql :freestyle)) &key)
  '())
