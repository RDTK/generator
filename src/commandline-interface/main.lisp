;;;; package.lisp --- Package definition for the commandline-interface module.
;;;;
;;;; Copyright (C) 2013, 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.project.commandline-interface)

;;; Input

(defun load-templates (files)
  (with-sequence-progress (:templates files)
    (mapcan (lambda (file)
              (progress "~S" file)
              (restart-case
                  (list (load-template/json file))
                (continue (&optional condition)
                  :report (lambda (stream)
                            (format stream "~@<Skip template ~
                                            specification ~S.~@:>"
                                    file))
                  (declare (ignore condition)))))
            files)))

(defun locate-projects (distribution-pathnames distributions)
  (remove-duplicates
   (mappend
    (lambda (distribution-pathname distribution)
      (let ((projects-directory
              (merge-pathnames
               "../projects/"
               (make-pathname :name     nil
                              :type     "project"
                              :defaults distribution-pathname))))
        (mapcan
         (lambda+ ((name &rest versions))
           (when-let ((location
                       (first
                        (locate-specifications
                         :project
                         (list (merge-pathnames name projects-directory))))))
             (list (list location versions distribution))))
         (jenkins.project::versions distribution))))
    distribution-pathnames distributions)
   :test #'equalp))

(defun analyze-project (project &key temp-directory)
  (let+ (((&labels+ do-branch ((branch . info))
            (let+ (((&plist-r/o (scm              :scm)
                                (branch-directory :branch-directory)
                                (versions         :versions)
                                (authors          :authors)
                                (description      :description)
                                (requires         :requires)
                                (provides         :provides)
                                (properties       :properties)) info)
                   (version (or (find branch (versions project) :key #'name :test #'string=)
                                (let ((version (make-instance 'version-spec
                                                              :name   branch
                                                              :parent project)))
                                  (push version (versions project))
                                  version)))
                   (version (reinitialize-instance
                             version
                             :requires  requires
                             :provides  provides
                             :variables (append
                                         (jenkins.project::%direct-variables version) ; TODO
                                         (when description
                                           (list :description description))
                                         (when authors
                                           (list :authors (mapcar (lambda (author)
                                                                    (ppcre:regex-replace-all "(@|\\$)" author "\\\\\\1"))
                                                                  authors)))
                                         (iter (for (key . value) in (append versions properties))
                                               (collect (make-keyword (string-upcase key)))
                                               (collect value))))))
              ;; TODO temp
              (iter (for job in (jobs project))
                    (pushnew (string-downcase scm) (tags job) :test #'string=))

              (setf (lookup version :branch) branch)
              (when branch-directory
                (setf (lookup version :branch-directory) branch-directory)))))
         ((&labels+ do-branch1 ((&whole arg branch . &ign))
            (restart-case
                (do-branch arg)
              (continue (&optional condition)
                :report (lambda (stream)
                          (format stream "~@<Skip branch ~A.~@:>" branch))
                (declare (ignore condition)))))))

        (handler-bind
            ((error (lambda (condition)
                      (error 'jenkins.analysis:analysis-error
                             :specification project
                             :cause         condition))))
          (mapc #'do-branch1
                (flet ((var (name &optional default)
                         (handler-case
                             (value project name)
                           (error () default))))
                  (apply #'jenkins.analysis:analyze
                         (when-let ((value (var :repository)))
                           (puri:uri value))
                         :auto
                         :scm           (var :scm)
                         :username      (var :scm.username)
                         :password      (var :scm.password)
                         :branches      (var :branches)
                         :tags          (var :tags)
                         :sub-directory (when-let ((value (var :sub-directory)))
                                          (parse-namestring (concatenate 'string value "/")))
                         :history-limit (var :scm.history-limit)
                         (append
                          (let ((natures (var :natures :none)))
                            (unless (eq natures :none)
                              (list :natures (mapcar (compose #'make-keyword #'string-upcase) natures))))
                          (when temp-directory
                            (list :temp-directory temp-directory))))))))
  project)

(defun load-projects/versioned (files-and-versions)
  (with-sequence-progress (:load/project files-and-versions)
    (lparallel:pmapcar
     (lambda+ ((file versions distribution))
       (restart-case
           (let* ((project  (reinitialize-instance
                             (load-project-spec/json
                              file :version-test (lambda (version)
                                                   (member version versions :test #'string=)))
                             :parent distribution))
                  (branches (intersection versions (ignore-errors (lookup project :branches))
                                          :test #'string=))
                  (tags     (intersection versions (ignore-errors (lookup project :tags))
                                          :test #'string=)))
             (unless (set-equal versions (union branches tags) :test #'string=)
               (error "~@<Could not find version~P ~{~S~^, ~} in project ~A.~@:>"
                      (length versions) versions project))
             (setf (lookup project :branches) branches
                   (lookup project :tags)     tags)
             project)
         (continue (&optional condition)
           :report (lambda (stream)
                     (format stream "~@<Skip project specification ~
                                      ~S.~@:>"
                             file))
           (declare (ignore condition)))))
     :parts most-positive-fixnum files-and-versions)))

(defun analyze-projects (projects &key temp-directory)
  (with-sequence-progress (:analyze/project projects)
    (lparallel:pmapcan
     (lambda (project)
       (restart-case
           (when-let ((project (apply #'analyze-project project
                                      (when temp-directory
                                        (list :temp-directory temp-directory)))))
             (list (setf (find-project (name project)) project)))
         (continue (&optional condition)
           :report (lambda (stream)
                     (format stream "~@<Skip analyzing project ~
                                      ~A.~@:>"
                             project))
           (declare (ignore condition)))))
     :parts most-positive-fixnum projects)))

(defun resolve-project-version (project version)
  (let ((project (find-project project)))
    (or (find version (versions project)
              :test #'string= :key #'name)
        (error "~@<Could not find version ~S in project ~A.~@:>"
               version project))))

(defun resolve-project-versions (versions)
  (mapcan (lambda+ ((project &rest versions))
            (restart-case
                (mapcar (curry #'resolve-project-version project)
                        versions)
              (continue (&optional condition)
                :report (lambda (stream)
                          (format stream "~@<Skip ~A.~@:>" version))
                (declare (ignore condition)))))
          versions))

(defun load-distributions (files &optional (overwrites '()))
  (with-sequence-progress (:load/distribution files)
    (mapcan (lambda (file)
              (progress "~S" file)
              (restart-case
                  (let ((distribution (load-distribution/json file)))
                    (iter (for (name . value) in overwrites)
                          (setf (lookup distribution name) value))
                    (list distribution))
                (continue (&optional condition)
                  :report (lambda (stream)
                            (format stream "~@<Skip distribution ~
                                            specification ~S.~@:>"
                                    file))
                  (declare (ignore condition)))))
            files)))

(defun check-platform-requirements
    (distributions
     &key
     (platform (multiple-value-list (jenkins.analysis:current-platform))))
  (let ((installed-packages (jenkins.analysis:installed-packages))
        (requirements       (platform-requires distributions platform)))
    (log:info "~@<Found ~:D installed package~:P~@:>"
              (length installed-packages))
    (log:debug "~@<Found ~:D platform requirement~:P: ~{~A~^ ~}~@:>"
               (length requirements) requirements)
    (when (and platform installed-packages)
      (dolist (requirement requirements)
        (restart-case
            (or (find requirement installed-packages
                      :test #'string= :key #'first)
                (error 'jenkins.analysis:unfulfilled-platform-dependency-error
                       :dependency requirement))
          (continue (&optional condition)
            :report (lambda (stream)
                      (format stream "~@<Ignore the requirement ~A and ~
                                      continue.~@:>"
                              requirement))
            (declare (ignore condition)))))))
  distributions)

(defun check-distribution-access (distributions)
  (mapcan (lambda (distribution)
            (restart-case
                (let+ (((&values access? problem)
                        (check-access distribution t)))
                  (cond
                    (access?
                     (list distribution))
                    (problem
                     (error problem))
                    (t
                     (error "~@<Unsuitable access declaration in ~
                             distribution ~A.~@:>"
                            distribution))))
              (continue (&optional condition)
                :report (lambda (stream)
                          (format stream "~@<Skip distribution ~A.~@:>"
                                  distribution))
                (declare (ignore condition)))))
          distributions))

;; Deployment

(defun instantiate-projects (specs
                             &optional
                             (distributions nil distributions-supplied?))
  (let+ ((projects (mapcar #'instantiate specs))
         ((&flet find-version (version)
            (when-let ((dist (find version distributions
                                   :test #'member
                                   :key  #'versions)))
              (remove-if-not (rcurry #'member (versions dist))
                             (providers/alist)
                             :key #'cdr)))))
    (iter (for project in projects)
          (for spec    in specs)
          (when project
            (apply #'add-dependencies! project spec
                   (when distributions-supplied?
                     (list :providers #'find-version)))
            (collect project)))))

(defun deploy-projects (projects)
  (let ((jobs
          (with-sequence-progress (:deploy/project projects)
            (iter (for project in projects)
                  (progress "~A" project)
                  (restart-case
                      (appending (flatten (deploy project)))
                    (continue (&optional condition)
                      :report (lambda (stream)
                                (format stream "~@<Skip deploying ~
                                                project ~S.~@:>"
                                        project))
                      (declare (ignore condition))))))))

    (with-sequence-progress (:deploy/dependencies jobs)
      (iter (for job in jobs)
            (progress "~A" job)
            (restart-case
                (deploy-dependencies job)
              (continue (&optional condition)
                :report (lambda (stream)
                          (format stream "~@<Skip deploying ~
                                          dependencies of job ~S.~@:>"
                                  job))
                (declare (ignore condition))))))

    jobs))

(defun enable-jobs (jobs)
  (with-sequence-progress (:enable jobs)
    (iter (for job in jobs)
          (progress "~S" job)
          (restart-case
              (jenkins.api:enable! (jenkins.api:job (jenkins.api:id job)))
            (continue (&optional condition)
              :report (lambda (stream)
                        (format stream "~@<Skip enabling job ~
                                        ~A.~@:>"
                                job))
              (declare (ignore condition)))))))

(defun generated-jobs ()
  (remove "automatically generated" (jenkins.api:all-jobs)
          :test-not #'search
          :key      #'jenkins.api:description))

;;; Toolkit specific stuff

(defun configure-buildflow-job (buildflow-job jobs
                                &key
                                prepare-name
                                finish-name
                                (ignore-failures? t))
  (let+ (((&labels format-flow (jobs)
            (etypecase jobs
              ((cons (eql :parallel))
               (format nil "parallel (~%~2@T~<~@;~{{~A}~^,~%~}~:>~%)"
                       (list (mapcar #'format-flow (rest jobs)))))
              ((cons (eql :serial))
               (format nil "~%~2@T~<~@;~{~A~^~%~}~:>~%"
                       (list (mapcar #'format-flow (rest jobs)))))
              (t
               (format nil "~:[~:;ignore(ABORTED) {~%~2@T~]~
                            build(~S, tag: build.id)~
                            ~2:*~:[~:;~%}~]"
                       ignore-failures? jobs)))))
         (script (format nil "~@[build(\"~A\", tag: build.id)~2%~]~
                              ~A~
                              ~@[~2%build(\"~A\", tag: build.id)~]"
                         prepare-name (format-flow jobs) finish-name)))
    (setf (jenkins.api::dsl buildflow-job) script)))

(defun schedule-jobs/serial (jobs)
  (let+ ((ordered (jenkins.project::sort-with-partial-order ; TODO
                   (copy-list jobs)
                   (lambda (left right)
                     (member left (direct-dependencies right)))))
         ((&flet job->id (job)
            (jenkins.api:id (implementation job)))))
    `(:serial ,@(mapcar #'job->id ordered))))

(defun schedule-jobs/parallel (jobs)
  (let+ (((&flet sort-jobs (jobs)
            (jenkins.project::sort-with-partial-order ; TODO
             (copy-list jobs)
             (lambda (left right)
               (member left (direct-dependencies right))))))
         ((&labels find-components (jobs)
            (let+ ((nodes (make-hash-table))
                   ((&flet add-job (job)
                      (let ((component '()))
                        (dolist (upstream (intersection jobs (list* job (dependencies job))))
                          (unionf component (gethash upstream nodes (list upstream)))
                          (dolist (member component)
                            (setf (gethash member nodes) component)))))))
              (mapc #'add-job jobs)
              (let ((components (mapcar #'sort-jobs
                                        (remove-duplicates
                                         (hash-table-values nodes)))))
                (cond
                  ((not (length= 1 components))
                   `(:parallel ,@(mapcar #'find-components components)))
                  ((length= 1 (first components))
                   (jenkins.api:id (implementation (first (first components)))))
                  (t
                   (let+ ((component (first components))
                          (index (floor (length component) 2))
                          (left (find-components (subseq component 0 index)))
                          (right (find-components (subseq component index)))
                          ((&flet splice (spec)
                             (if (typep spec '(cons (eql :serial)))
                                 (rest spec)
                                 (list spec)))))
                     `(:serial ,@(splice left) ,@(splice right))))))))))
    (find-components jobs)))

(define-constant +description-automatically-generated+
  "!!! This job is automatically generated - do not modify by hand. !!!"
  :test #'string=)

(defun configure-jobs (distribution jobs
                       &key build-flow-ignores-failures?)
  (macrolet ((ensure-job ((kind name &key (commit? t)) &body body)
               `(let ((job (jenkins.dsl:job (,kind ,name) ,@body)))
                  (if (jenkins.api:job? (jenkins.api:id job))
                      (setf (jenkins.api:job-config (jenkins.api:id job))
                            (jenkins.api::%data job))
                      (jenkins.api::make-job (jenkins.api:id job) (jenkins.api::%data job)))
                  (setf (jenkins.api:description job)
                        +description-automatically-generated+)
                  ,@(when commit?
                      '((jenkins.api:commit! job)
                        (jenkins.api:enable! job)))
                  job)))
    (let+ ((buildflow-name       (ignore-errors
                                  (value distribution :buildflow-name)))
           (buildflow-parallel?  (handler-case
                                     (value distribution :buildflow.parallel?)
                                   (error (c) t)))
           (prepare-name         (ignore-errors
                                  (value distribution :prepare-hook-name)))
           (prepare-command      (ignore-errors
                                  (value distribution :prepare-hook/unix)))
           (finish-name          (ignore-errors
                                  (value distribution :finish-hook-name)))
           (finish-command       (ignore-errors
                                  (value distribution :finish-hook/unix)))
           (finish-command       (when finish-command
                                   (format nil "jobs='~{~A~^~%~}'~2%~A"
                                           (mapcar (compose #'jenkins.api:id
                                                            #'implementation)
                                                   jobs)
                                           finish-command)))
           ((&flet make-hook-job (name command)
              (when name
                (ensure-job ("project" name)
                  (jenkins.api:builders
                   (jenkins.dsl::shell (:command (or command
                                                     "# <nothing to do>")))))))))
      (make-hook-job prepare-name prepare-command)
      (make-hook-job finish-name finish-command)

      ;; Create bluildflow job
      (when buildflow-name
        (let ((schedule (funcall (if buildflow-parallel?
                                     #'schedule-jobs/parallel
                                     #'schedule-jobs/serial)
                                 jobs))
              (job      (ensure-job ('("com.cloudbees.plugins.flow.BuildFlow"
                                       "build-flow-plugin@0.10")
                                      buildflow-name
                                      :commit? nil))))
          (configure-buildflow-job
           job schedule
           :prepare-name     prepare-name
           :finish-name      finish-name
           :ignore-failures? build-flow-ignores-failures?)
          (jenkins.api:commit! job)
          (jenkins.api:enable! job))))))

(defun configure-distribution (distribution
                               &key
                               (build-flow-ignores-failures? t))
  (unless (ignore-errors (value distribution :disable-ochestration-jobs))
    (let ((jobs (mappend (compose #'jobs #'implementation) (versions distribution))))
      (log:trace "~@<Jobs in ~A: ~A~@:>" distribution jobs)
      (configure-jobs distribution jobs
                      :build-flow-ignores-failures? build-flow-ignores-failures?))))

(defun configure-distributions (distributions
                                &key
                                (build-flow-ignores-failures? t))
  (mapc (rcurry #'configure-distribution
                :build-flow-ignores-failures? build-flow-ignores-failures?)
        distributions))

(defun list-credentials (jobs)
  (let+ ((all-credentials (make-hash-table :test #'equal))
         ((&flet+ job-credentials (job)
            (when-let* ((repository  (jenkins.api:repository job))
                        (credentials (jenkins.api:credentials repository)))
              (push job (gethash credentials all-credentials))))))
    (mapc #'job-credentials jobs)
    (when (plusp (hash-table-count all-credentials))
      (format t "~@<The following credentials have been used:~@:_~
                 ~{~{* ~S for job~P ~<~{~A~^, ~}~:@>~}~^~@:_~}~
                 ~%~:>"
              (mapcar (lambda+ ((credentials . jobs))
                        (list credentials (length jobs)
                              (list (mapcar #'jenkins.api:id jobs))))
                      (hash-table-alist all-credentials))))))

;;; Commandline options

(defun update-synopsis ()
  "Create and return a commandline option tree."
  (clon:make-synopsis
   ;; Basic usage and specific options.
   :item    (clon:defgroup (:header "General Options")
              (flag   :long-name     "version"
                      :description
                      "Print version information and exit.")
              (flag   :long-name     "help"
                      :short-name    "h"
                      :description
                      "Print this help and exit.")
              (flag   :long-name     "swank"
                      :description
                      "Start a swank server.")
              (flag   :long-name     "debug"
                      :description
                      "Enable debug mode.")
              (enum   :long-name     "progress-style"
                      :enum          '(:cmake :vertical)
                      :default-value :vertical
                      :description
                      "Progress display style.")
              (lispobj :long-name    "num-processes"
                       :short-name   "j"
                       :typespec     'positive-integer
                       :default-value 8
                       :argument-name "NUMBER-OF-PROCESSES"
                       :description
                       "Number of processes to execute in parallel when checking out from repositories and analyzing working copies.")
              (enum    :long-name     "on-error"
                       :enum          '(:abort :continue)
                       :argument-name "POLICY"
                       :default-value :abort
                       :description
                       "Abort when encountering errors? Either \"abort\" or \"continue\".")
              (path    :long-name    "temp-directory"
                       :type         :directory
                       :argument-name "DIRECTORY"
                       :default-value #P"/tmp/"
                       :description
                       "Directory into which temporary files should be written during analysis step."))

   :item    (clon:defgroup (:header "Jenkins Options")
              (stropt :long-name     "template"
                      :short-name    "t"
                      :argument-name "TEMPLATE"
                      :description
                      "Load one or more templates. This option can be supplied multiple times.")
              (stropt :long-name     "distribution"
                      :short-name    "d"
                      :argument-name "DISTRIBUTION"
                      :description
                      "Load one or more distributions. This option can be supplied multiple times.")
              (stropt :long-name     "set"
                      :short-name    "D"
                      :argument-name "VARIABLE-NAME=VALUE"
                      :description
                      "Overwrite a variable after loading the distribution. Arguments to this option have to be of the form VARIABLE-NAME=VALUE. This option can be supplied multiple times.")
              (stropt :long-name     "base-uri"
                      :short-name    "b"
                      :argument-name "URI"
                      :default-value "https://localhost:8080"
                      :description
                      "Jenkins base URI.")
              (stropt :long-name     "username"
                      :short-name    "u"
                      :description
                      "Username for Jenkins authentication.")
              (stropt :long-name     "password"
                      :short-name    "p"
                      :description
                      "Password for Jenkins authentication.")
              (stropt :long-name     "api-token"
                      :short-name    "a"
                      :description
                      "API token for Jenkins authentication.")
              (flag   :long-name     "delete-other"
                      :description
                      "Delete previously automatically generated jobs when they are not re-created in this generation run.")
              (flag   :long-name     "build-flow-fail"
                      :description
                      "Configure build-flow to fail when one of the jobs coordinated by it fails."))

   :item    (clon:defgroup (:header "Drupal Options")
              (stropt :long-name     "drupal-base-uri"
                      :argument-name "URI"
                      :default-value "https://toolkit.cit-ec.uni-bielefeld.de"
                      :description
                      "Drupal base URI.")
              (stropt :long-name     "drupal-username"
                      :description
                      "Username for Drupal authentication.")
              (stropt :long-name     "drupal-password"
                      :description
                      "Password for Drupal authentication."))))

(defun collect-inputs (spec)
  (cond
    ((wild-pathname-p spec)
     (directory spec))
    ((pathnamep spec)
     (list spec))
    (t
     (error "~@<Invalid input specification: ~S.~@:>" spec))))

;;; Error handling stuff

(defun call-with-delayed-error-reporting (thunk
                                          &key
                                          debug?
                                          (report-function #'report-error))
  (let+ ((errors      '())
         (errors-lock (bt:make-lock))
         ((&flet errors ()
            (bt:with-lock-held (errors-lock)
              (copy-list errors))))
         ((&flet (setf errors) (new-value)
            (bt:with-lock-held (errors-lock)
              (setf errors new-value))))
         ((&flet collect-error (condition)
            (when debug?
              (terpri)
              (princ condition)
              (terpri)
              (sb-debug:print-backtrace))
            (bt:with-lock-held (errors-lock)
              (appendf errors (list condition)))))
         ((&flet report ()
            (mapc (lambda (condition)
                    (ignore-errors
                     (funcall report-function *error-output* condition))
                    (format *error-output* "~2%"))
                  errors)))
         ((&flet call-with-deferrable-conditions (thunk)
            (restart-bind ((defer (lambda (condition)
                                    (collect-error condition)
                                    (continue)
                                    (abort))
                             :test-function (lambda (condition)
                                              (find-restart 'continue condition))))
              (funcall thunk)))))
    (unwind-protect ; TODO probably not a good idea
         ;; Execute THUNK collecting errors.
         (lparallel:task-handler-bind ((error (lambda (condition)
                                                (call-with-deferrable-conditions
                                                 (lambda () (error condition))))))
           (call-with-deferrable-conditions
            (lambda () (funcall thunk #'errors #'(setf errors) #'report))))

      ;; Report collected errors.
      (report))))

(defmacro with-delayed-error-reporting ((&key debug? report-function)
                                        &body body)
  (with-unique-names (errors set-errors report)
    `(call-with-delayed-error-reporting
      (lambda (,errors ,set-errors ,report)
        (flet ((errors () (funcall ,errors))
               ((setf errors) (new-value) (funcall ,set-errors new-value))
               (report () (funcall ,report)))
          ,@body))
      ,@(when debug? `(:debug? ,debug?))
      ,@(when report-function `(:report-function ,report-function)))))

;;; Main

(defun collect-option-values (&rest args &key &allow-other-keys)
  (iter (for spec next (apply #'clon:getopt args))
        (while spec)
        (collect spec)))

(defun locate-specifications (kind namestrings)
  (restart-case
      (or (iter (for namestring in namestrings)
                (if-let ((matches (collect-inputs (parse-namestring namestring))))
                  (appending matches)
                  (warn "~@<~A pattern ~S did not match anything.~@:>"
                        kind namestring)))
          (error "~@<None of the ~A patterns ~{~S~^, ~} matched ~
                  anything.~@:>"
                 kind namestrings))
    (continue (&optional condition)
      :report (lambda (stream)
                (format stream "~@<Continue without loading ~A ~
                                specifications.~@:>"
                        kind))
      (declare (ignore condition))
      '())))

(defun parse-overwrite (spec)
  (let+ (((variable &optional value)
          (split-sequence:split-sequence #\= spec :count 2)))
    (when (emptyp variable)
      (error "~@<Empty variable name in ~S.~@:>" spec))
    (cons (make-keyword (string-upcase variable)) value)))

(defun call-with-phase-error-check (phase errors set-errors report continuable?
                                    thunk)
  (prog1
      (funcall thunk)
    (when-let* ((errors       (funcall errors))
                (phase-errors (remove-if (of-type 'phase-condition) errors)))
      (restart-case
          (error 'simple-phase-error
                 :phase            phase
                 :format-control   "~@<~D error~:P during ~A phase.~@[
                                    This error is fatal.~]~@:>"
                 :format-arguments (list (length phase-errors) phase
                                         (not continuable?)))
        (continue (&optional condition)
          :report (lambda (stream)
                    (format stream "~@<Ignore the error:P in phase ~A ~
                                    and continue.~@:>"
                            (length phase-errors) phase))
          :test   (lambda (condition)
                    (declare (ignore condition))
                    continuable?)
          (declare (ignore condition))
          (funcall set-errors
                   (append (set-difference errors phase-errors)
                           (list (make-condition 'deferred-phase-error
                                                 :phase      phase
                                                 :conditions phase-errors))))
          (funcall report)
          (funcall set-errors '()))))))

(defmacro with-phase-error-check ((phase errors set-errors report
                                   &key
                                   (continuable? 't))
                                  &body body)
  `(call-with-phase-error-check
    ',phase ,errors ,set-errors ,report ,continuable? (lambda () ,@body)))

(defun main ()
  (update-synopsis)
  (clon:make-context)
  (when (clon:getopt :long-name "help")
    (clon:help)
    (uiop:quit))
  (when (clon:getopt :long-name "version")
    (let ((version (asdf:component-version (asdf:find-system :jenkins.project))))
      (format *standard-output* "~A version ~:[~{~D.~D~^.~D~^-~A~}~;~A~]~&"
              "build-generator" (stringp version) version))
    (uiop:quit))

  (let+ ((debug?                 (clon:getopt :long-name "debug"))
         (*print-right-margin*   (if-let ((value (sb-posix:getenv "COLUMNS")))
                                   (parse-integer value)
                                   200))
         (num-processes          (clon:getopt :long-name "num-processes"))
         ((&flet restart/condition (name)
            (lambda (condition)
              (when-let ((restart (find-restart name condition)))
                (invoke-restart restart condition)))))
         (error-policy           (case (clon:getopt :long-name "on-error")
                                   (:continue #'continue)
                                   (t         (restart/condition 'abort))))
         (effective-error-policy (lambda (condition)
                                   (cond
                                     ((and (typep condition 'simple-phase-error)
                                           (funcall error-policy condition)
                                           nil))
                                     ((funcall (restart/condition 'defer) condition))
                                     ((funcall (restart/condition 'abort) condition)))))
         (temp-directory         (clon:getopt :long-name "temp-directory")))
    (log:config :thread (if debug? :trace :warn))

    (restart-case

        (lparallel:task-handler-bind ((error effective-error-policy))
          (handler-bind ((error effective-error-policy))
            (with-delayed-error-reporting (:debug? debug?)

              (let* ((jenkins.api:*base-url*       (clon:getopt :long-name "base-uri"))
                     (jenkins.api:*username*       (clon:getopt :long-name "username"))
                     (jenkins.api:*password*       (or (clon:getopt :long-name "password")
                                                       (clon:getopt :long-name "api-token")))
                     (delete-other?                (clon:getopt :long-name "delete-other"))
                     (build-flow-ignores-failures? (not (clon:getopt :long-name "build-flow-fail")))

                     (templates     (with-phase-error-check
                                        (:locate/template #'errors #'(setf errors) #'report)
                                      (sort (locate-specifications
                                             :template (collect-option-values :long-name "template"))
                                            #'string< :key #'pathname-name)))
                     (distributions (with-phase-error-check
                                        (:locate/distribution #'errors #'(setf errors) #'report)
                                      (locate-specifications
                                       :distribution (collect-option-values :long-name "distribution"))))
                     (overwrites    (mapcar #'parse-overwrite
                                            (collect-option-values :long-name "set"))))

                (setf lparallel:*kernel* (lparallel:make-kernel num-processes))

                (with-trivial-progress (:jobs)

                  (let* ((templates         (with-phase-error-check
                                                (:load/template #'errors #'(setf errors) #'report)
                                              (load-templates templates)))
                         (distributions/raw (with-phase-error-check
                                                (:load/distribution #'errors #'(setf errors) #'report)
                                              (load-distributions distributions overwrites)))
                         (projects          (with-phase-error-check
                                                (:locate/project #'errors #'(setf errors) #'report)
                                              (locate-projects distributions distributions/raw)))
                         (projects/raw      (with-phase-error-check
                                                (:load/project #'errors #'(setf errors) #'report)
                                              (load-projects/versioned projects)))
                         (projects/specs    (with-phase-error-check
                                                (:analyze/project #'errors #'(setf errors) #'report)
                                              (apply #'analyze-projects projects/raw
                                                     (when temp-directory
                                                       (list :temp-directory temp-directory)))))
                         (distributions     (with-phase-error-check
                                                (:resolve/distribution #'errors #'(setf errors) #'report)
                                              (mapcar (lambda (distribution)
                                                        (reinitialize-instance
                                                         distribution
                                                         :versions (resolve-project-versions
                                                                    (jenkins.project::versions distribution))))
                                                      distributions/raw)))
                         (distributions     (with-phase-error-check
                                                (:check-platform-requirements #'errors #'(setf errors) #'report)
                                              (check-platform-requirements distributions)))
                         (distributions     (with-phase-error-check
                                                (:check-access #'errors #'(setf errors) #'report
                                                 :continuable? nil)
                                              (check-distribution-access distributions)))
                         (projects          (with-phase-error-check
                                                (:instantiate/project #'errors #'(setf errors) #'report)
                                              (instantiate-projects projects/specs distributions)))
                         (jobs/spec         (with-phase-error-check
                                                (:deploy/project #'errors #'(setf errors) #'report)
                                              (flatten (deploy-projects projects))))
                         (jobs              (mappend #'implementations jobs/spec)))
                    (declare (ignore templates))

                    ;; Delete automatically generated jobs found on the
                    ;; server for which no counterpart exists among the newly
                    ;; generated jobs. This is necessary to get rid of
                    ;; leftover jobs when projects (or project versions) are
                    ;; deleted or renamed.
                    (when delete-other?
                      (let ((other-jobs (set-difference (generated-jobs) jobs
                                                        :key #'jenkins.api:id :test #'string=)))
                        (with-sequence-progress (:delete-other other-jobs)
                          (mapc (progressing #'jenkins.api::delete-job :delete-other)
                                other-jobs))))

                    ;; TODO explain
                    (with-trivial-progress (:orchestration "Configuring orchestration jobs")
                      (configure-distributions
                       distributions
                       :build-flow-ignores-failures? build-flow-ignores-failures?))

                    (enable-jobs jobs)

                    (with-phase-error-check
                        (:list-credentials #'errors #'(setf errors) #'report)
                      (list-credentials jobs))))))))

      (abort (&optional condition)
        :report (lambda (stream)
                  (format stream "~@<Abort execution.~@:>"))
        (when condition
          (report-error *error-output* condition))
        (sb-ext:exit :code 1)))))
