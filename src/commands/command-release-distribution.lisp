;;;; command-release-distribution.lisp --- Write a distribution release into a recipe.
;;;;
;;;; Copyright (C) 2017, 2018, 2019 Jan Moringen
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
   #.(format nil "Write a distribution release into a new recipe file.~@
      ~@
      Revision information for project versions included in the ~
      distribution is retrieved from the most recent builds of ~
      corresponding Jenkins jobs.~@
      ~@
      Note: this only works reliably if a mode and overwrites matching ~
      the values used to generate the jobs are supplied since things ~
      like Jenkins job names depend on this information.")))

(service-provider:register-provider/class
 'command :release-distribution :class 'release-distribution)

(jenkins.project.commandline-options:define-option-mapping
    (*command-schema* "release-distribution")
  (&rest                  "distributions" "DISTRIBUTION-NAME"   t)

  (("--mode" "-m")        "mode"          "MODE")
  (("--set" "-D")         "overwrites"    "VARIABLE-NAME=VALUE")

  (("--base-uri" "-b")    "base-uri"      "URI")
  (("--username" "-u")    "username"      "LOGIN")
  (("--password" "-p")    "password"      "PASSWORD")
  (("--api-token" "-a")   "api-token"     "API-TOKEN")

  (("--output-file" "-o") "output-file"   "OUTPUT-FILENAME"     t)) ; TODO make this positional?

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

    (release-distribution command distribution output-file)))

(defun release-distribution (command distribution output-file)
  (let* ((location (jenkins.model.project::location-of (specification distribution)))
         (source   (text.source-location:source location))
         (jobs     (mappend (lambda (version)
                              (when-let ((job (find "main" (jobs version)
                                                    :test #'string= :key #'name))) ; TODO otherwise error?
                                (list (cons version job))))
                            (versions distribution)))
         (results  (as-phase (:inspect-jobs)
                     (inspect-jobs jenkins.api:*base-url*
                                   jenkins.api:*username*
                                   jenkins.api:*password*
                                   jobs))))
    ;; Output description of the jobs' build statuses.
    (format-distribution-build-status *standard-output* results)
    ;; Output new distribution file.
    (with-output-to-file (stream output-file :if-exists :supersede)
      (format-new-content stream command distribution jobs results source))))

;;; Inspecting jobs

(defun inspect-jobs (base-url username password jobs)
  (let ((results (make-hash-table :test #'eq :synchronized t)))
    (with-sequence-progress (:retrieve-build-data jobs)
      (lparallel:pmapc
       (lambda+ ((version . job))
         (progress "~/print-items:format-print-items/"
                   (print-items:print-items job))
         (with-simple-restart (continue "Skip job ~A" job)
           (let ((jenkins.api:*base-url* base-url)
                 (jenkins.api:*username* username)
                 (jenkins.api:*password* password))
             (setf (gethash version results) (inspect-job job)))))
       jobs))
    results))

(defun inspect-job (job)
  (let+ ((id         (jenkins.model.project::jenkins-job-id job))
         (scm        (value/cast job :scm ""))
         (build-id   (format nil "~A/lastBuild" id))
         ((&values build result) (handler-case
                                     (let ((build (jenkins.api:build build-id)))
                                       (values build (if (jenkins.api:building? build)
                                                         :building
                                                         (jenkins.api:result build))))
                                   (error ()
                                     (values nil nil))))
         (build-data (cond ((not build)
                            nil)
                           ((eq scm :git)
                            (when-let ((data (jenkins.api::action-of-type
                                              'jenkins.api::action/git-build-data build)))
                              (jenkins.api::last-built-revision/sha1 data)))
                           ((eq scm :svn)
                            (when-let ((data (jenkins.api::action-of-type
                                              'jenkins.api::action/subversion-change-log-set build)))
                              (jenkins.api::revision data)))
                           (t
                            :unsupported-scm))))
    (list id result build-data)))

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
                (:success                 32)
                (:unstable                33)
                ((nil :building :failure) 31)
                (t                        34))))
    (call-with-sgr (lambda (stream)
                     (format stream "~8<~(~A~)~;~>" (or result "no build")))
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
  (declare (ignore job-id))
  (format stream "~32A ~32A ~
                  ~/jenkins.project.commands::format-result/ ~
                  ~/jenkins.project.commands::format-commit/ ~%"
          (name (parent (specification version))) (name version)
          result commit))

(defun format-distribution-build-status (stream data)
  (loop :with sorted = (sort (hash-table-alist data) #'string<
                             :key (compose #'name #'parent #'specification #'car))
        :for (version . (job-id result data)) :in sorted
        :do (format-build-data stream version job-id result data)))

;;; Output

(defun compute-changes (distribution jobs results source)
  (loop :for (version . job)    :in jobs
        :for spec               = (jenkins.model:specification version)
        :for include            = (find spec (jenkins.model.project:versions
                                              (jenkins.model:specification distribution))
                                        :test #'eq :key #'jenkins.model.project:version)
        :for parameters         = (jenkins.model.variables:direct-variables include)
        :for (nil nil revision) = (gethash version results)
        :for location           = (jenkins.model.project::location-of include)
        :if (not (eq (text.source-location:source location) source))
          :do (log:warn "Skipping ~A" location)
        :else
          :collect (list (text.source-location:index
                          (text.source-location:start location))
                         (text.source-location:index
                          (text.source-location:end location))
                         (format nil "name: ~A~@
                                        ~2@Tversion: ~A~@
                                        ~2@Tparameters:~@
                                          ~4@Tcommit: ~A~
                                          ~@[~@
                                            ~{~4@T~(~A~): ~A~^~%~}~
                                          ~]"
                                 (name (parent spec))
                                 (name spec)
                                 revision
                                 (alist-plist parameters)))))

(defun apply-changes (text changes)
  (reduce (lambda+ (text (start end string))
            (when (char= (aref text (1- end)) #\Newline)
              (decf end))
            (concatenate 'string
                         (subseq text 0 start)
                         string
                         (subseq text end)))
          (sort changes #'> :key #'first) :initial-value text))

(defun format-header (stream command)
  (let ((*print-right-margin* 80))
    (format stream "# This file has been generated automatically:~@
                    #~@
                    #  User        ~A~@
                    #  Host        ~A~@
                    #  Date        ~A~@
                    #  Generator   ~A~@
                    #  Commandline ~{'~A'~^ ~}~@
                    #~@
                    #  Jenkins URL ~A~@
                    #  Input file~P  ~{~A~^ ~}~@
                    #  Mode        ~A~%~@[~
                    #  Overwrites~@
                    ~{#    ~(~36A~) → ~:A~%~}~
                    ~]"
            (sb-unix:uid-username (sb-posix:getuid))
            (machine-instance)
            (local-time:now)
            (generator-version)
            (uiop:command-line-arguments)

            (base-uri command)
            (length (distributions command)) (distributions command)
            (mode command)
            (alist-plist (overwrites command)))))

(defun format-new-content (stream command distribution jobs results source)
  (let* ((text    (text.source-location:content source))
         (changes (compute-changes distribution jobs results source))
         (body    (apply-changes text changes)))
    (format-header stream command)
    (format stream "~2%")
    (write-string body stream)))
