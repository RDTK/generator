;;;; command-release-distribution.lisp --- Write a distribution release into a recipe.
;;;;
;;;; Copyright (C) 2017, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.project.commands)

(defclass release-distribution (distribution-input-mixin
                                mode-mixin
                                jenkins-access-mixin)
  ((output-file :initarg :output-file
                :type    configuration.options:file-pathname
                :reader  output-file))
  (:default-initargs
   :output-file (missing-required-initarg 'release-distribution :output-file))
  (:documentation
   "Write a distribution release into a new recipe file."))

(service-provider:register-provider/class
 'command :release-distribution :class 'release-distribution)

(jenkins.project.commandline-options:define-option-mapping
    (*command-schema* "release-distribution")
  (&rest                  "distributions"        "DISTRIBUTION-NAME"   t)

  (("--mode" "-m")        "mode"                 "MODE")
  (("--set" "-D")         "overwrites"           "VARIABLE-NAME=VALUE")

  (("--base-uri" "-b")    "base-uri"             "URI")
  (("--username" "-u")    "username"             "LOGIN")
  (("--password" "-p")    "password"             "PASSWORD")
  (("--api-token" "-a")   "api-token"            "API-TOKEN")

  (("--output-file" "-o") "output-file"          "OUTPUT-FILENAME"     t)) ; TODO make this positional?

(defmethod command-execute ((command release-distribution))
  (let+ (((&accessors-r/o distributions mode overwrites output-file) command)
         ((&values distributions projects)
          (generate-load distributions mode overwrites
                         :generator-version (generator-version)))
         (distributions
          (generate-analyze distributions projects
                            :generator-version (generator-version)
                            :cache-directory   *cache-directory*
                            :temp-directory    *temp-directory*))
         (distributions
          (as-phase (:instantiate)
            (mapcan (lambda (distribution-spec)
                      (when-let ((distribution (instantiate distribution-spec)))
                        (list distribution)))
                    distributions)))
         (distribution (first distributions)))
    ; TODO (generate-check distributions)

    (release-distribution distribution output-file)))

(defun release-distribution (distribution output-file)
  (let* ((location (jenkins.model.project::location-of (specification distribution)))
         (text     (text.source-location:content
                    (text.source-location:source location))))

    (describe distribution)
    (describe (specification distribution))

    (let ((jobs (mappend (lambda (version)
                           (when-let ((job (find "main" (jobs version)
                                                 :test #'string= :key #'name))) ; TODO otherwise error?
                             (list (cons version job))))
                         (versions distribution)))

          (base-url jenkins.api:*base-url*)
          (username jenkins.api:*username*)
          (password jenkins.api:*password*)
          (results  (make-hash-table :test #'eq :synchronized t)))
      (with-sequence-progress (:retrieve-build-data jobs)
        (lparallel:pmapc
         (lambda+ ((version . job))
           (progress "~A" job)
           (with-simple-restart (continue "Skip job ~A" job)
             (let ((jenkins.api:*base-url* base-url)
                   (jenkins.api:*username* username)
                   (jenkins.api:*password* password))
               (setf (gethash version results) (inspect-job job)))))
         jobs))

      (loop :for (version . (job-id result data)) :in (sort (hash-table-alist results) #'string<
                                                            :key (compose #'name #'parent #'specification #'car))
            :do (format-build-data *standard-output* version job-id result data)))

    (let ((new-text ))
      (write-string-into-file text output-file :if-exists :supersede))))

(defun inspect-job (job)
  (let* ((id         (jenkins.model.project::jenkins-job-id job))
         (scm        (value/cast job :scm ""))
         (build-id   (format nil "~A/lastBuild" id))
         (build      (jenkins.api::build build-id))
         (build-data (cond ((eq scm :git)
                            (when-let ((data (jenkins.api::action-of-type
                                              'jenkins.api::action/git-build-data build)))
                              (jenkins.api::last-built-revision/sha1 data)))
                           ((eq scm :svn)
                            (when-let ((data (jenkins.api::action-of-type
                                              'jenkins.api::action/subversion-change-log-set build)))
                              (jenkins.api::revision data)))
                           (build
                            :unsupported-scm)
                           (t
                            nil))))
    (list id (jenkins.api:result build) build-data)))

;;; Result display

(defun call-with-sgr (thunk stream code)
  (unwind-protect
       (progn
         (format stream "~C[~Dm" #\Escape code)
         (funcall thunk stream))
    (format stream "~C[0m" #\Escape)))

(defun format-result (stream result &optional colon? at?)
  (declare (ignore colon? at?))
  (let ((code (case result
                (:success  32)
                (:unstable 33)
                (:failure  31)
                (t         34))))
    (call-with-sgr (lambda (stream)
                     (format stream "~8<~A~;~>" result))
                   stream code)))

(defun format-commit (stream commit &optional colon? at?)
  (declare (ignore colon? at?))
  (let+ (((&values variant code)
          (case commit
            (:unsupported-scm (values 0 0))
            ((nil)            (values 1 31))
            (t                (values 2 1)))))
    (call-with-sgr (lambda (stream)
                     (format stream "~[«unsupported scm»~*~;«missing»~*~;~A~]"
                             variant commit))
                   stream code)))

(defun format-build-data (stream version job-id result commit)
  (format stream "~32A ~32A ~
                  ~/jenkins.project.commands::format-result/ ~
                  ~/jenkins.project.commands::format-commit/ ~%"
          (name (parent (specification version))) (name version)
          result commit))
