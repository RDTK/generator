;;;; analysis.lisp ---
;;;;
;;;; Copyright (C) 2012, 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.analysis)

(defun guess-project-natures (directory)
  (or (append
       (when (directory (merge-pathnames "*.asd" directory) )
         '(:asdf))

       (when (probe-file (merge-pathnames "setup.py" directory) )
         '(:setuptools))

       (when (probe-file (merge-pathnames "pom.xml" directory) )
         '(:maven))

       (when (probe-file (merge-pathnames "build.xml" directory) )
         '(:ant))

       (cond
         ((and (probe-file (merge-pathnames "CMakeLists.txt" directory))
               (directory (merge-pathnames "c*/*ebian*.cmake" directory)))
          '(:cmake/debian))

         ((probe-file (merge-pathnames "CMakeLists.txt" directory))
          '(:cmake))))

      '(:freestyle)))

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
                    &key
                    (natures nil natures-supplied?)
                    branches)
  (let ((natures (cond
                   (natures-supplied?
                    natures)
                   ((guess-project-natures source))
                   (t
                    (error "~@<Could not automatically determine the ~
                            project nature of ~S.~@:>"
                           source)))))
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
             (appendf (gethash key merged) (ensure-list value))))
         (hash-table-plist merged))))))

(defmethod analyze ((source pathname) (kind cons) &rest args &key)
  (mapcan (lambda (kind)
            (restart-case
                (list (apply #'analyze source kind args))
              (continue (&optional condition)
                :report (lambda (stream)
                          (format stream "~@<Do not analyze ~A with ~
                                          nature ~A~@:>"
                                  source kind))
                (declare (ignore condition)))))
          kind))

(defmethod analyze ((source pathname) (kind (eql :freestyle))
                    &key)
  '())
