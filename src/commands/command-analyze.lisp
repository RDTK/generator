;;;; command-analyze.lisp --- Analyze a project repository.
;;;;
;;;; Copyright (C) 2017, 2018, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:build-generator.commands)

(deftype nature ()
  '(member :freestyle :maven :cmake :asdf :ros-package :ros-packages))

(defclass analyze ()
  ((sources :initarg  :sources
            :type     (or null (cons (or configuration.options::proper-puri pathname) list))
            :reader   sources
            :documentation
            #.(format nil "Location(s) of the project repository(ies).~@
               ~@
               For repository locations specified as URIs, the ~
               following syntax is used:~@
               ~@
               ~2@TSCHEMA://HOST[:PORT][/PATH][#BRANCH][?scm=SCM&sub-directory=SUB-DIRECTORY]~@
               ~@
               where SCM is \"git\", \"subversion\", \"mercurial\" or ~
               \"archive\", BRANCH is the branch and SUB-DIRECTORY is ~
               the sub-directory within the repository that that ~
               should be analyzed. All other components of the URI are ~
               passed to respective version control system."))
   (natures :initarg  :natures
            :type     (or null (cons nature list))
            :reader   natures
            :initform nil
            :documentation
            #.(format nil "Nature(s) according to which the project(s) ~
               should be analyzed.~@
               ~@
               The project nature(s) is/are guessed if not ~
               specified.")))
  (:documentation
   #.(format nil "Analyze project repositories w.r.t. dependencies ~
      and meta-data.~@
      ~@
      Analysis results are printed to the standard output stream in ~
      JSON format.")))

(service-provider:register-provider/class
 'command :analyze :class 'analyze)

(build-generator.commandline-options:define-option-mapping
    (*command-schema* "analyze")
  (("--nature" "-n") "natures" "NATURE")
  (&rest             "sources" "URI-OR-DIRECTORY" t))

(defmethod command-execute ((command analyze))
  (let+ (((&accessors-r/o sources natures) command)
         (results (as-phase (:analyze)
                    (with-sequence-progress (:analyze sources)
                      (lparallel:pmapcan
                       (progressing
                        (lambda (input)
                          (with-simple-restart
                              (continue "~@<Skip ~A.~@:>" input)
                            (list (cons (princ-to-string input)
                                        (more-conditions::without-progress
                                          (analyze-input input :natures natures))))))
                        :analyze)
                       sources))))
         (stream  *standard-output*))
    (if (length= 1 results)
        (json:encode-json-plist (cdr (first results)) stream)
        (json:with-object (stream)
          (map nil (lambda+ ((input . result))
                     (json:as-object-member (input stream)
                       (json:encode-json-plist result stream)))
               results)))
    (terpri stream)))

;;; Analysis

(defgeneric analyze-input (input &key natures)

  (:method ((input puri:uri) &key natures)
    (let+ ((branch                      (or (puri:uri-fragment input)
                                            "master"))
           ((&values scm sub-directory) (uri-scm-and-sub-directory input))
           (uri                         (uri-without-branch-scm-sub-directory
                                         input)))
      (first
       (apply #'analysis:analyze uri :auto
              :versions `((:branch ,branch))
              (append
               (when scm
                 `(:scm ,scm))
               (when sub-directory
                 `(:sub-directory ,sub-directory))
               (when natures
                 `(:natures ,natures)))))))

  (:method ((input pathname) &key natures)
    (let ((input (uiop:ensure-directory-pathname input)))
      (apply #'analysis:analyze input :auto
             :history-limit 1
             (when natures
               `(:natures ,natures))))))

;;; Utilities

(defun uri-parse-query (uri)
  (when-let ((query (puri:uri-query uri)))
    (loop :for key-start = 0 :then (1+ value-end)
          :for key-end = (position #\= query :start (1+ key-start))
          :for value-start = (1+ key-end)
          :for value-end = (or (position #\& query :start value-start)
                               (length query))
          :collect (make-keyword (subseq query key-start key-end))
          :into parameters
          :collect (subseq query value-start value-end)
          :into parameters
          :when (= value-end (length query))
          :do (return parameters))))

(defun uri-unparse-query (alist)
  (format nil "~{~A=~A~^&~}" alist))

(defun uri-without-branch-scm-sub-directory (uri)
  (let ((query (uri-unparse-query
                (remove-from-plist
                 (uri-parse-query uri) :|scm| :|sub-directory|))))
    (apply #'make-instance 'puri:uri
           :scheme (puri:uri-scheme uri)
           :host   (puri:uri-host uri)
           :port   (puri:uri-port uri)
           :path   (puri:uri-path uri)
           (when query
             (list :query query)))))

(defun uri-scm-and-sub-directory (uri)
  (let ((parsed-query (uri-parse-query uri)))
    (values
     (getf parsed-query :|scm|)
     (when-let ((sub-directory (getf parsed-query :|sub-directory|)))
       (uiop:ensure-directory-pathname sub-directory)))))
