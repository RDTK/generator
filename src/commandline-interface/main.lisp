;;;; main.lisp --- Entry-point of commandline-interface module.
;;;;
;;;; Copyright (C) 2013, 2014, 2015, 2016, 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.project.commandline-interface)

;;; Input

(defun load-templates (files)
  (with-sequence-progress (:templates files)
    (mapcan (lambda (file)
              (progress "~S" file)
              (with-simple-restart
                  (continue "~@<Skip template specification ~S.~@:>" file)
                (list (load-template/json file))))
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
                         (list (make-pathname :name     name
                                              :defaults projects-directory))))))
             (list (list location versions distribution))))
         (jenkins.model.project::versions distribution))))
    distribution-pathnames distributions)
   :test #'equalp))

(defstruct project-spec-and-versions
  (spec     nil :type jenkins.model.project::project-spec :read-only t)
  (versions nil :type list                                :read-only t))

(defmethod print-items:print-items append ((object project-spec-and-versions))
  (let+ (((&structure-r/o project-spec-and-versions- spec versions) object)
         (versions (mapcar (rcurry #'getf :name) versions)))
    (append (print-items:print-items spec)
            `((:versions ,versions ":~{~A~^,~}" ((:after :name)))))))

(defun analyze-project (project &key cache-directory temp-directory non-interactive)
  (let+ (((&structure-r/o project-spec-and-versions- (project spec) versions) project)
         ((&labels+ do-version ((version-info . info))
            (let+ ((version-name      (getf version-info :name))
                   (version-variables (remove-from-plist version-info :name))
                   ((&plist-r/o (scm              :scm)
                                (branch-directory :branch-directory)
                                (authors          :authors)
                                (description      :description)
                                (requires         :requires)
                                (provides         :provides)
                                (properties       :properties))
                    info)
                   (version (or (find version-name (versions project)
                                      :key #'name :test #'string=)
                                (let ((version (make-instance 'version-spec
                                                              :name   version-name
                                                              :parent project)))
                                  (push version (versions project))
                                  version))))
              (reinitialize-instance
               version
               :requires  requires
               :provides  provides
               :variables (append
                           (jenkins.model.variables:direct-variables version) ; TODO
                           (apply #'value-acons (append version-variables '(())))
                           (when scm
                             (list (value-cons :scm (string-downcase scm))))
                           (when branch-directory
                             (list (value-cons :branch-directory branch-directory)))
                           (when description
                             (list (value-cons :description description)))
                           (when authors
                             (list (value-cons
                                    :authors (mapcar (lambda (author)
                                                       (ppcre:regex-replace-all "(@|\\$)" author "\\\\\\1"))
                                                     authors))))
                           (iter (for (key . value) in properties)
                                 (collect (value-cons (make-keyword (string-upcase key))
                                                      value))))))))
         ((&labels+ do-version1 ((&whole arg version-info . &ign))
            (with-simple-restart
                (continue "~@<Skip version ~A.~@:>" version-info)
              (do-version arg)))))

    (handler-bind
        ((error (lambda (condition)
                  (error 'jenkins.analysis:analysis-error
                         :specification project
                         :cause         condition))))
      (mapc #'do-version1
            (flet ((var (name &optional default)
                     (value project name default)))
              (apply #'jenkins.analysis:analyze
                     (when-let ((value (var :repository)))
                       (puri:uri value))
                     :auto
                     :scm              (var :scm)
                     :username         (var :scm.username)
                     :password         (var :scm.password)
                     :versions         versions
                     :sub-directory    (when-let ((value (var :sub-directory)))
                                         (parse-namestring (concatenate 'string value "/")))
                     :history-limit    (var :scm.history-limit)
                     :non-interactive  non-interactive
                     (append
                      (let ((natures (var :natures :none)))
                        (unless (eq natures :none)
                          (list :natures (mapcar (compose #'make-keyword #'string-upcase) natures))))
                      (when cache-directory
                        (list :cache-directory cache-directory))
                      (when temp-directory
                        (list :temp-directory temp-directory)))))))
    project))

(defun load-projects/versioned (files-and-versions)
  (with-sequence-progress (:load/project files-and-versions)
    (lparallel:pmapcan
     (lambda+ ((file versions distribution))
       (progress "~A" file)
       (with-simple-restart
           (continue "~@<Skip project specification ~S.~@:>" file)
         (let+ ((project       (reinitialize-instance
                                (load-project-spec/json
                                 file :version-test (lambda (version)
                                                      (member version versions
                                                              :test #'string=)))
                                :parent distribution))
                (branches      (as (value project :branches '()) 'list))
                (branches      (intersection versions branches :test #'string=))
                (tags          (as (value project :tags '()) 'list))
                (tags          (intersection versions tags :test #'string=))
                (tags+branches (union branches tags))
                (versions1     (set-difference versions tags+branches
                                               :test #'string=))
                ((&flet process-version (name &key version-required? branch? tag? directory?)
                   (let+ ((version (or (find name (versions project)
                                             :test #'string= :key #'name)
                                       (when version-required?
                                         (error "~@<No version section ~
                                                 for version ~S in ~
                                                 project ~A.~@:>"
                                                name project))))
                          ((&flet version-var (name &optional default)
                             (if version
                                 (value version name default)
                                 default)))
                          (branch    (when branch?    (version-var :branch (when (eq branch? t) name))))
                          (tag       (when tag?       (version-var :tag (when (eq tag? t) name))))
                          (directory (when directory? (version-var :directory)))
                          (commit    (version-var :commit)))
                     `(:name   ,name
                       ,@(when branch    `(:branch    ,branch))
                       ,@(when tag       `(:tag       ,tag))
                       ,@(when directory `(:directory ,directory))
                       ,@(when commit    `(:commit    ,commit)))))))
           (list (make-project-spec-and-versions
                  :spec     project
                  :versions (append (mapcar (rcurry #'process-version :branch? t) branches)
                                    (mapcar (rcurry #'process-version :tag?    t) tags)
                                    (mapcar (rcurry #'process-version
                                                    :version-required? t
                                                    :branch?           :maybe
                                                    :tag?              :maybe
                                                    :directory?        :mabye)
                                            versions1)))))))
     :parts most-positive-fixnum files-and-versions)))

(defun analyze-projects (projects &key cache-directory temp-directory non-interactive)
  (jenkins.analysis::with-git-cache ()
    (let ((cache jenkins.analysis::*git-cache*))
      (with-sequence-progress (:analyze/project projects)
        (lparallel:pmapcan
         (lambda (project)
           (progress "~/print-items:format-print-items/"
                     (print-items:print-items project))
           (more-conditions::without-progress
             (let ((jenkins.analysis::*git-cache* cache))
               (with-simple-restart
                   (continue "~@<Skip analyzing project ~A.~@:>" project)
                 (when-let ((project (apply #'analyze-project project
                                            :non-interactive non-interactive
                                            (append
                                             (when cache-directory
                                               (list :cache-directory cache-directory))
                                             (when temp-directory
                                               (list :temp-directory temp-directory))))))
                   (list (setf (find-project (name project)) project)))))))
         :parts most-positive-fixnum projects)))))

(defun resolve-project-version (project version)
  (let ((project (find-project project)))
    (or (find version (versions project) :test #'string= :key #'name)
        (error "~@<Could not find version ~S in project ~A.~@:>"
               version project))))

(defun resolve-project-versions (versions)
  (mapcan (lambda+ ((project &rest versions))
            (mapcan (lambda (version)
                      (with-simple-restart
                          (continue "~@<Skip version ~A of project ~A.~@:>"
                                    version project)
                        (list (resolve-project-version project version))))
                    versions))
          versions))

(defun load-distributions (files &optional (overwrites '()))
  (with-sequence-progress (:load/distribution files)
    (mapcan (lambda (file)
              (progress "~S" file)
              (with-simple-restart
                  (continue "~@<Skip distribution specification ~S.~@:>" file)
                (let ((distribution (load-distribution/json file)))
                  (iter (for (name . value) in overwrites)
                        (log:info "~@<In ~A, setting ~S to ~S.~@:>"
                                  distribution name value)
                        (setf (lookup distribution name) value))
                  (list distribution))))
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
        (with-simple-restart
            (continue "~@<Ignore the requirement ~A.~@:>" requirement)
          (or (find requirement installed-packages
                    :test #'string= :key #'first)
              (error 'jenkins.analysis:unfulfilled-platform-dependency-error
                     :dependency requirement))))))
  distributions)

(defun check-distribution-access (distributions)
  (mapcan (lambda (distribution)
            (with-simple-restart
                (continue "~@<Skip distribution ~A.~@:>" distribution)
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
                          distribution))))))
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
  (with-sequence-progress (:deploy/project projects)
    (iter (for project in projects)
          (progress "~/print-items:format-print-items/"
                    (print-items:print-items project))
          (more-conditions::without-progress
            (with-simple-restart
                (continue "~@<Skip deploying project ~S.~@:>" project)
              (appending (flatten (deploy project))))))))

(defun deploy-job-dependencies (jobs)
  (with-sequence-progress (:deploy/dependencies jobs)
    (iter (for job in jobs)
          (progress "~/print-items:format-print-items/"
                    (print-items:print-items job))
          (deploy-dependencies job))))

(defun enable-jobs (jobs)
  (with-sequence-progress (:enable jobs)
    (iter (for job in jobs)
          (progress "~S" (jenkins.api:id job))
          (with-simple-restart
              (continue "~@<Skip enabling job ~A.~@:>" job)
            (jenkins.api:enable! (jenkins.api:job (jenkins.api:id job)))))))

(defun generated-jobs (&optional pattern)
  (remove "automatically generated" (apply #'jenkins.api:all-jobs
                                           (when pattern (list pattern)))
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
                            build(~S, tag: buildId)~
                            ~2:*~:[~:;~%}~]"
                       ignore-failures? jobs)))))
         (script (format nil "buildId = build.time.format('yyyy-MM-dd_HH-mm-ss')~2%~
                              ~@[build(\"~A\", tag: buildId)~2%~]~
                              ~A~
                              ~@[~2%build(\"~A\", tag: buildId)~]"
                         prepare-name (format-flow jobs) finish-name)))
    (setf (jenkins.api::dsl buildflow-job) script)))

(defun schedule-jobs/serial (jobs)
  (let+ ((ordered (jenkins.model::sort-with-partial-order ; TODO
                   (copy-list jobs)
                   (lambda (left right)
                     (member left (direct-dependencies right)))))
         ((&flet job->id (job)
            (jenkins.api:id (implementation job)))))
    `(:serial ,@(mapcar #'job->id ordered))))

(defun schedule-jobs/parallel (jobs)
  (let+ (((&flet sort-jobs (jobs)
            (jenkins.model::sort-with-partial-order ; TODO
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
    (let+ ((buildflow-name      (as (value distribution :buildflow-name) 'string))
           (buildflow-parallel? (as (value distribution :buildflow.parallel? t) 'boolean))

           (prepare-name        (as (value distribution :prepare-hook-name nil) '(or null string)))
           (prepare-command     (as (value distribution :prepare-hook/unix nil) '(or null string)))

           (finish-name         (as (value distribution :finish-hook-name nil) '(or null string)))
           (finish-command      (as (value distribution :finish-hook/unix nil) '(or null string)))
           (finish-command      (when finish-command
                                  (format nil "jobs='~{~A~^~%~}'~2%~A"
                                          (mapcar (compose #'jenkins.api:id
                                                           #'implementation)
                                                  jobs)
                                          finish-command)))
           ((&flet include-job? (job)
              (not (as (value job :buildflow.exclude? nil) 'boolean))))
           ((&flet make-hook-job (name command)
              (progress :orchestration nil "~A" name)
              (ensure-job ("project" name)
                (jenkins.api:properties
                 (jenkins.dsl:parameters (:parameters '((:kind :text :name "tag")))))
                (jenkins.api:builders
                 (jenkins.dsl:shell (:command (or command
                                                  "# <nothing to do>"))))))))
      (append
       ;; Maybe create hook jobs
       (when prepare-name
         (list (make-hook-job prepare-name prepare-command)))

       (when finish-name
         (list (make-hook-job finish-name  finish-command)))

       ;; Create bluildflow job
       (when buildflow-name
         (progress :orchestration nil "~A" buildflow-name)
         (let ((schedule (funcall (if buildflow-parallel?
                                      #'schedule-jobs/parallel
                                      #'schedule-jobs/serial)
                                  (remove-if-not #'include-job? jobs)))
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
           (jenkins.api:enable! job)
           (list job)))))))

(defun configure-distribution (distribution
                               &key
                               (build-flow-ignores-failures? t))
  (unless (as (value distribution :disable-orchestration-jobs nil) 'boolean)
    (let ((jobs (mappend (compose #'jobs #'implementation) (versions distribution))))
      (log:trace "~@<Jobs in ~A: ~A~@:>" distribution jobs)
      (configure-jobs distribution jobs
                      :build-flow-ignores-failures? build-flow-ignores-failures?))))

(defun configure-distributions (distributions
                                &key
                                (build-flow-ignores-failures? t))
  (mapcan (rcurry #'configure-distribution
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
      (format t "~@<The following credentials have been referenced and ~
                 have to be configured in Jenkins' credential store:~@:_~
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
              (flag    :long-name     "version"
                       :description
                       "Print version information and exit.")
              (flag    :long-name     "help"
                       :short-name    "h"
                       :description
                       "Print this help and exit.")
              (flag    :long-name     "swank"
                       :description
                       "Start a swank server.")
              (flag    :long-name     "debug"
                       :description
                       "Enable debug mode.")
              (enum    :long-name     "progress-style"
                       :enum          '(:none :cmake :one-line)
                       :description
                       "Progress display style.")
              (flag    :long-name    "non-interactive"
                       :description
                       "Avoid any user interaction.")
              (lispobj :long-name    "num-processes"
                       :short-name   "j"
                       :typespec     'positive-integer
                       :argument-name "NUMBER-OF-PROCESSES"
                       :description
                       "Number of processes to execute in parallel when checking out from repositories and analyzing working copies.")
              (enum    :long-name     "on-error"
                       :enum          '(:abort :continue)
                       :argument-name "POLICY"
                       :description
                       "Abort when encountering errors? Either \"abort\" or \"continue\".")
              (path    :long-name    "cache-directory"
                       :type         :directory
                       :argument-name "DIRECTORY"
                       :description
                       "Directory into which repository mirrors should be written.")
              (path    :long-name    "temp-directory"
                       :type         :directory
                       :argument-name "DIRECTORY"
                       :description
                       "Directory into which temporary files should be written during analysis step.")
              (path    :long-name    "report-directory"
                       :type         :directory
                       :argument-name "DIRECTORY"
                       :description
                       "Write information about distributions and projects into one or more report files. The written information includes most of the content of the respective underlying recipe but also expanded variable values, inferred variable values and analysis results.")
              (flag    :long-name    "dry-run"
                       :description
                       "Read recipes and perform the usual analysis but do not create or delete Jenkins jobs.")
              (stropt  :long-name    "trace-variable"
                       :argument-name "VARIABLE-NAME"
                       :description
                       "Trace all accesses to the specified variable."))

   :item    (clon:defgroup (:header "Jenkins Options")
              (path   :long-name     "template-directory"
                      :type          :directory
                      :argument-name "DIRECTORY"
                      :description
                      "Directory containing sub-directories in turn containing template files. Must be used in combination with the mode option to select one of the sub-directories.")
              (stropt :long-name     "template"
                      :short-name    "t"
                      :argument-name "TEMPLATE"
                      :description
                      "Load one or more templates. This option can be supplied multiple times. Mutually exclusive with the mode option.")
              (stropt :long-name     "distribution"
                      :short-name    "d"
                      :argument-name "DISTRIBUTION"
                      :description
                      "Load one or more distributions. This option can be supplied multiple times.")
              (stropt :long-name     "mode"
                      :short-name    "m"
                      :argument-name "MODE"
                      :description
                      "The mode according to which jobs should be generated. Selects a sub-directory of the directory specified using the template-directory option and thus a set of templates. Mutually exclusive with the template option.")
              (stropt :long-name     "set"
                      :short-name    "D"
                      :argument-name "VARIABLE-NAME=VALUE"
                      :description
                      "Overwrite a variable after loading the distribution. Arguments to this option have to be of the form VARIABLE-NAME=VALUE. This option can be supplied multiple times.")
              (stropt :long-name     "base-uri"
                      :short-name    "b"
                      :argument-name "URI"
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
              (stropt :long-name     "delete-other-pattern"
                      :argument-name "REGEX"
                      :description
                      "When deleting previously automatically generated jobs, only consider jobs whose name matches the regular expression REGEX.

A common case, deleting only jobs belonging to the distribution being generated, can be achieved using the regular expression DISTRIBUTION-NAME$.")
              (flag   :long-name     "build-flow-fail"
                      :description
                      "Configure build-flow to fail when one of the jobs coordinated by it fails."))))

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
              (bt:with-lock-held (errors-lock)
                (terpri)
                (princ condition)
                (terpri)
                (sb-debug:print-backtrace)))
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

(defun locate-specifications (kind namestrings)
  (with-simple-restart (continue "~@<Do not load ~A specifications.~@:>" kind)
    (or (iter (for namestring in namestrings)
              (if-let ((matches (collect-inputs (parse-namestring namestring))))
                (appending matches)
                (warn "~@<~A pattern ~S did not match anything.~@:>"
                      kind namestring)))
        (error "~@<None of the ~A patterns ~{~S~^, ~} matched ~
                anything.~@:>"
               kind namestrings))))

(defun parse-overwrite (spec)
  (let+ ((position  (or (position #\= spec)
                        (error "~@<Variable assignment ~S is not of the ~
                                form NAME=VALUE.~@:>"
                               spec)))
         (name/raw  (subseq spec 0 position))
         (name      (make-keyword (string-upcase name/raw)))
         (value/raw (subseq spec (1+ position)))
         (value     (if (and (not (emptyp value/raw))
                             (member (aref value/raw 0) '(#\" #\{ #\[)))
                        (let ((json::*json-identifier-name-to-lisp* #'string-upcase))
                          (json:decode-json-from-string value/raw))
                        value/raw)))
    (cons name value)))

(defun call-with-phase-error-check (phase errors set-errors report continuable?
                                    thunk)
  (let ((start (get-internal-real-time)))
    (format t "START ~A~%" phase)
    (unwind-protect
         (prog1
             (funcall thunk)
           (when-let* ((errors       (funcall errors))
                       (phase-errors (remove-if (of-type 'phase-condition) errors)))
             (restart-case
                 (error 'simple-phase-error
                        :phase            phase
                        :format-control   "~@<~D error~:P during ~A phase.~@[ ~
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
                 (terpri)
                 (funcall report)
                 (funcall set-errors '())))))
      (let ((end (get-internal-real-time)))
        (format t "~&END   ~A, ~,3F second~:P~2%"
                phase
                (/ (- end start)
                   internal-time-units-per-second))))))

(defmacro with-phase-error-check ((phase errors set-errors report
                                   &key
                                   (continuable? 't))
                                  &body body)
  `(call-with-phase-error-check
    ',phase ,errors ,set-errors ,report ,continuable? (lambda () ,@body)))

(defun configure ()
  (let+ ((*print-right-margin*   (if-let ((value (sb-posix:getenv "COLUMNS"))) ; TODO
                                   (parse-integer value)
                                   100))
         (schema        *schema*)
         (configuration (configuration.options:make-configuration schema))
         (source        (configuration.options.sources:make-source
                         :common-cascade ; TODO environment variables
                         :basename "build-generator"
                         :syntax   :ini))
         (synchronizer  (make-instance 'configuration.options:standard-synchronizer
                                       :target configuration))
         ((&flet option-value (section name)
            (let+ ((option (configuration.options:find-option
                            (list section name) configuration))
                   ((&values value source)
                    (if (typep (configuration.options:option-type option)
                               '(cons (eql list)))
                        (iter (for spec next (clon:getopt :long-name name))
                              (while spec)
                              (collect spec :into values)
                              (finally (when values (return (values values t)))))
                        (clon:getopt :long-name name))))
              (if source
                  (values value :commandline)
                  (configuration.options:option-value
                   option :if-does-not-exist nil))))))
    ;; Process configuration options.
    (handler-case
        (progn
          (configuration.options.sources:initialize source schema)
          (configuration.options.sources:process source synchronizer))
      (error (condition)
        (format t "Configuration error:~%~A~%" condition)
        (uiop:quit 3)))

    ;; Process commandline options.
    (update-synopsis)
    (clon:make-context)

    (let ((debug? (option-value "general" "debug")))

      (when debug?
        (describe configuration)
        (fresh-line))

      (when (or (emptyp (uiop:command-line-arguments))
                (option-value "general" "help"))
        (clon:help)
        (uiop:quit))

      (when (option-value "general" "version")
        (let ((version (or (uiop:symbol-call '#:jenkins.project-system '#:version/list
                                             :revision? t :commit? t)
                           (asdf:component-version (asdf:find-system :jenkins.project)))))
          (format *standard-output* "~A version ~:[~{~D.~D~^.~D~^-~A~}~;~A~]~&"
                  "build-generator" (stringp version) version))
        (uiop:quit))

      (values #'option-value configuration debug?))))

(defun main ()
  (let+ (((&values option-value &ign debug?) (configure))
         ((&flet option-value (&rest args)
            (apply option-value args)))
         (progress-style         (option-value "general" "progress-style"))
         (*print-right-margin*   (if-let ((value (sb-posix:getenv "COLUMNS")))
                                   (parse-integer value)
                                   200))
         (non-interactive        (option-value "general" "non-interactive"))
         (num-processes          (option-value "general" "num-processes"))
         ((&flet restart/condition (name)
            (lambda (condition)
              (when-let ((restart (find-restart name condition)))
                (invoke-restart restart condition)))))
         (non-dependency-errors? nil)
         (error-policy           (case (option-value "general" "on-error")
                                   (:continue #'continue)
                                   (t         (restart/condition 'abort))))
         (effective-error-policy (lambda (condition)
                                   (when (typep condition
                                                '(and error
                                                      (not unfulfilled-project-dependency-error)))
                                     (setf non-dependency-errors? t))
                                   (cond
                                     ((and (typep condition 'simple-phase-error)
                                           (funcall error-policy condition)
                                           nil))
                                     ((funcall (restart/condition 'defer) condition))
                                     ((funcall (restart/condition 'abort) condition)))))
         (cache-directory        (option-value "general" "cache-directory"))
         (temp-directory         (option-value "general" "temp-directory"))
         (report-directory       (option-value "general" "report-directory"))
         (dry-run?               (option-value "general" "dry-run"))

         (main (bt:current-thread))
         (lock (bt:make-lock)))
    (log:config :thread (if debug? :trace :warn))

    (restart-case

        (handler-bind ((error effective-error-policy)
                       (more-conditions:progress-condition
                         (lambda (condition)
                           (sb-sys:without-interrupts
                             (bt:with-lock-held (lock)
                               (case progress-style
                                 (:none)
                                 (:cmake
                                  (princ condition)
                                  (fresh-line))
                                 (:one-line
                                  (let* ((progress      (progress-condition-progress condition))
                                         (progress/real (progress->real progress))
                                         (width    20))
                                    (format t "~C[2K[~VA] ~A~C[G"
                                            #\Escape
                                            width
                                            (make-string (floor progress/real (/ width))
                                                         :initial-element #\#)
                                            condition
                                            #\Escape)
                                    (if (eq progress t)
                                        (terpri)
                                        (force-output))))))))))
          (lparallel:task-handler-bind ((error effective-error-policy)
                                        (more-conditions:progress-condition
                                         (lambda (condition)
                                           (bt:interrupt-thread
                                            main (lambda () (signal condition))))))
            (with-delayed-error-reporting (:debug? debug?)

              (let+ ((jenkins.api:*base-url*       (option-value "jenkins" "base-uri"))
                     (jenkins.api:*username*       (option-value "jenkins" "username"))
                     (jenkins.api:*password*       (or (option-value "jenkins" "password")
                                                       (option-value "jenkins" "api-token")))
                     (delete-other?                (option-value "generation" "delete-other"))
                     (delete-other-pattern         (option-value "generation" "delete-other-pattern"))
                     (build-flow-ignores-failures? (not (option-value "generation" "build-flow-fail")))
                     (template-directory           (option-value "generation" "template-directory"))
                     (template                     (option-value "generation" "template"))
                     ((&values mode mode?)         (option-value "generation" "mode"))
                     (distribution                 (option-value "generation" "distribution"))
                     (template-pattern             (cond
                                                     ((and template mode (not (eq mode? :default)))
                                                      (error "~@<The options template and mode are mutually exclusive.~@:>"))
                                                     (template)
                                                     (mode
                                                      (let ((template-directory (or template-directory
                                                                                    (make-pathname
                                                                                     :name     nil
                                                                                     :type     nil
                                                                                     :defaults (merge-pathnames
                                                                                                #P"../templates/"
                                                                                                (first distribution))))))
                                                        (list (merge-pathnames
                                                               (make-pathname
                                                                :name      :wild
                                                                :type      "template"
                                                                :directory `(:relative ,mode))
                                                               template-directory))))
                                                     (t
                                                      (error "~@<At least one of the options template and mode must be supplied.~@:>"))))

                     (templates     (with-phase-error-check
                                        (:locate/template #'errors #'(setf errors) #'report)
                                      (sort (locate-specifications :template template-pattern)
                                            #'string< :key #'pathname-name)))
                     (distributions (with-phase-error-check
                                        (:locate/distribution #'errors #'(setf errors) #'report)
                                      (locate-specifications :distribution distribution)))
                     (overwrites    (mapcar #'parse-overwrite (option-value "generation" "set"))))
                (setf *traced-variables* (mapcar (compose #'make-keyword #'string-upcase)
                                                 (option-value "general" "trace-variable")))
                (setf lparallel:*kernel* (lparallel:make-kernel num-processes))

                (with-trivial-progress (:jobs)

                  (let* ((templates          (with-phase-error-check
                                                 (:load/template #'errors #'(setf errors) #'report
                                                  :continuable? nil)
                                               (load-templates templates)))
                         (distributions/raw  (with-phase-error-check
                                                 (:load/distribution #'errors #'(setf errors) #'report
                                                  :continuable? nil)
                                               (load-distributions distributions overwrites)))
                         (projects           (with-phase-error-check
                                                 (:locate/project #'errors #'(setf errors) #'report)
                                               (locate-projects distributions distributions/raw)))
                         (projects/raw       (with-phase-error-check
                                                 (:load/project #'errors #'(setf errors) #'report)
                                               (load-projects/versioned projects)))
                         (projects/specs     (with-phase-error-check
                                                 (:analyze/project #'errors #'(setf errors) #'report)
                                               (apply #'analyze-projects projects/raw
                                                      :non-interactive non-interactive
                                                      (append
                                                       (when cache-directory
                                                         (list :cache-directory cache-directory))
                                                       (when temp-directory
                                                         (list :temp-directory temp-directory))))))
                         (distributions      (with-phase-error-check
                                                 (:resolve/distribution #'errors #'(setf errors) #'report)
                                               (mapcar (lambda (distribution)
                                                         (reinitialize-instance
                                                          distribution
                                                          :versions (resolve-project-versions
                                                                     (jenkins.model.project::versions distribution))))
                                                       distributions/raw)))
                         (distributions      (with-phase-error-check
                                                 (:check-platform-requirements #'errors #'(setf errors) #'report)
                                               (check-platform-requirements distributions)))
                         (distributions      (with-phase-error-check
                                                 (:check-access #'errors #'(setf errors) #'report
                                                  :continuable? nil)
                                               (check-distribution-access distributions)))
                         (projects           (with-phase-error-check
                                                 (:instantiate/project #'errors #'(setf errors) #'report)
                                               (instantiate-projects projects/specs distributions)))
                         (jobs/spec          (unless dry-run?
                                               (with-phase-error-check
                                                   (:deploy/project #'errors #'(setf errors) #'report)
                                                 (let ((jobs (deploy-projects projects)))
                                                   (when (some (lambda (job)
                                                                 (not (or (as (value job :no-dependencies nil) 'boolean) ; TODO remove
                                                                          (string= (as (value job :dependencies.mode) 'string) "none"))))
                                                               jobs)
                                                     (deploy-job-dependencies jobs))
                                                   jobs))))
                         (jobs               (unless dry-run?
                                               (mappend #'implementations jobs/spec)))
                         (orchestration-jobs (unless dry-run?
                                               (with-phase-error-check
                                                   (:orchestration #'errors #'(setf errors) #'report)
                                                 (with-trivial-progress (:orchestration "Configuring orchestration jobs")
                                                   (with-simple-restart
                                                       (continue "~@<Continue without configuring orchestration jobs~@:>")
                                                     (configure-distributions
                                                      distributions
                                                      :build-flow-ignores-failures? build-flow-ignores-failures?)))))))
                    (declare (ignore templates))

                    (unless dry-run?
                      ;; Delete automatically generated jobs found on
                      ;; the server for which no counterpart exists
                      ;; among the newly generated jobs. This is
                      ;; necessary to get rid of leftover jobs when
                      ;; projects (or project versions) are deleted or
                      ;; renamed.
                      (when delete-other?
                        (with-phase-error-check
                            (:delete-other-jobs #'errors #'(setf errors) #'report)
                          (let ((other-jobs (set-difference
                                             (generated-jobs delete-other-pattern)
                                             (append jobs orchestration-jobs)
                                             :key #'jenkins.api:id :test #'string=)))
                            (with-sequence-progress (:delete-other other-jobs)
                              (mapc (progressing #'jenkins.api::delete-job :delete-other)
                                    other-jobs)))))

                      (with-phase-error-check
                          (:enable-jobs #'errors #'(setf errors) #'report)
                        (enable-jobs jobs))

                      (with-phase-error-check
                          (:list-credentials #'errors #'(setf errors) #'report)
                        (list-credentials jobs)))

                    (when report-directory
                      (with-phase-error-check
                          (:report #'errors #'(setf errors) #'report)
                        (flet ((maybe-first (thing)
                                 (if (consp thing) (first thing) thing)))
                          (jenkins.report:report
                           (maybe-first distributions) :json report-directory)
                          (with-simple-restart (continue "Skip graph report")
                            (if (setf cl-dot:*dot-path* (cl-dot::find-dot))
                                (jenkins.report:report
                                 (maybe-first distributions) :graph report-directory)
                                (error "~@<Could not find dot program.~@:>"))))))))))))

      (abort (&optional condition)
        :report (lambda (stream)
                  (format stream "~@<Abort execution.~@:>"))
        (when condition
          (report-error *error-output* condition))
        (uiop:quit 2)))
    (uiop:quit (if non-dependency-errors? 1 0))))
