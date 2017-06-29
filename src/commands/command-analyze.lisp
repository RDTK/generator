;;;; command-analyze.lisp --- Analyze a project repository.
;;;;
;;;; Copyright (C) 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.project.commands)

(defclass analyze ()
  ((sources :initarg  :sources
            :type     (or null (cons (or puri:uri pathname) list))
            :reader   sources
            :documentation
            "Location(s) of the project repository(ies).")
   (natures :initarg  :natures
            :type     (or null (cons (member :maven :cmake :asdf :ros-packages) list))
            :reader   natures
            :initform nil
            :documentation
            #.(format nil "List of natures according to which the ~
               project(s) should be analyzed.")))
  (:documentation
   "Analyze project repositories w.r.t. to dependencies and meta-data."))

(service-provider:register-provider/class
 'command :analyze :class 'analyze)

(jenkins.project.commandline-options:define-option-mapping
    (*command-schema* "analyze")
  (("--nature" "-n") "natures" "NATURE")
  (&rest             "sources" "URI-OR-DIRECTORY" t))

(defmethod command-execute ((command analyze))
  (let+ (((&accessors-r/o sources natures) command))
    (map nil (lambda (input)
               (json:encode-json-plist
                (analyze-input input :natures natures)))
         sources)))

;;; Analysis

(defgeneric analyze-input (input &key natures)

  (:method ((input puri:uri) &key natures)
    (let* ((branch        (or (puri:uri-fragment input)
                              "master"))
           (sub-directory (uri-sub-directory input))
           (uri           (uri-without-branch-and-sub-directory input)))
      (first                           ; TODO
       (apply #'jenkins.analysis:analyze uri :auto
              :versions `((:branch ,branch))
              (append
               (when sub-directory
                 `(:sub-directory ,sub-directory))
               (when natures
                 `(:natures ,natures)))))))

  (:method ((input pathname) &key natures)
    (let ((input (uiop:ensure-directory-pathname input)))
      (apply #'jenkins.analysis:analyze input :auto
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

(defun uri-without-branch-and-sub-directory (uri)
  (let ((query (uri-unparse-query
                (remove-from-plist
                 (uri-parse-query uri) :|sub-directory|))))
    (apply #'make-instance 'puri:uri
           :scheme (puri:uri-scheme uri)
           :host   (puri:uri-host uri)
           :port   (puri:uri-port uri)
           :path   (puri:uri-path uri)
           (when query
             (list :query query)))))

(defun uri-sub-directory (uri)
  (when-let ((sub-directory (getf (uri-parse-query uri) :|sub-directory|)))
    (uiop:ensure-directory-pathname sub-directory)))
