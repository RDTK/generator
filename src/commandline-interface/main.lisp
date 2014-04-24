;;;; package.lisp --- Package definition for the commandline-interface module.
;;;;
;;;; Copyright (C) 2013, 2014 Jan Moringen
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

(defun analyze-project (project)
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

    (mapc #'do-branch1 (jenkins.analysis:analyze
                        (puri:uri (value project :repository)) :auto
                        :scm           (ignore-errors (value project :scm))
                        :username      (ignore-errors (value project :scm.username))
                        :password      (ignore-errors (value project :scm.password))
                        :branches      (ignore-errors (value project :branches))
                        :tags          (ignore-errors (value project :tags))
                        :sub-directory (when-let ((value
                                                   (ignore-errors
                                                    (value project :sub-directory))))
                                         (parse-namestring (concatenate 'string value "/"))))))

  project)

(defun load-projects (files)
  (with-sequence-progress (:load/project files)
    (mapcan
     (lambda (project)
       (when project
         (list (setf (find-project (name project)) project))))
     (lparallel:pmapcar
      (lambda (file)
        (restart-case
            (analyze-project (load-project-spec/json file))
          (continue (&optional condition)
            :report (lambda (stream)
                      (format stream "~@<Skip project specification ~
                                      ~S.~@:>"
                              file))
            (declare (ignore condition)))))
      :parts most-positive-fixnum files))))

(defun load-distributions (files)
  (with-sequence-progress (:load/distribution files)
    (mapcan (lambda (file)
              (progress "~S" file)
              (restart-case
                  (list (load-distribution/json file))
                (continue (&optional condition)
                  :report (lambda (stream)
                            (format stream "~@<Skip distribution ~
                                            specification ~S.~@:>"
                                    file))
                  (declare (ignore condition)))))
            files)))

(defun tag-project-versions (distributions)
  (mapc (lambda (distribution)
          (mapc (lambda (version)
                  (handler-case (lookup version :distributions) (error () (setf (lookup version :distributions) '()))) ; TODO
                  (push (name distribution) (lookup version :distributions))
                  (handler-case (lookup version :variant-parents) (error () (setf (lookup version :variant-parents) '()))) ; TODO
                  (push distribution (lookup version :variant-parents))) ; TODO hack
                (versions distribution)))
        distributions)
  distributions)

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

(defun configure-buildflow-job (version jobs &key (ignore-failures? t))
  (let+ ((name   (format nil "toolkit-buildflow-~A" version))
         (job    (first (jenkins.api:all-jobs name)))
         ((&labels format-flow (jobs)
            (etypecase jobs
              ((cons (eql :parallel))
               (format nil "parallel (~%~2@T~<~@;~{{~A}~^,~%~}~:>~%)"
                       (list (mapcar #'format-flow (rest jobs)))))
              ((cons (eql :serial))
               (format nil "~%~2@T~<~@;~{~A~^~%~}~:>~%"
                       (list (mapcar #'format-flow (rest jobs)))))
              (t
               (format nil "~:[~:;ignore(FAILURE) {~%~2@T~]build(~S, tag: build.id)~2:*~:[~:;~%}~]"
                       ignore-failures? jobs)))))
         (script (format nil "build(\"toolkit-prepare-~A\", tag: build.id)~2%~
                              ~A~2%~
                              build(\"toolkit-finish-~2:*~A\", tag: build.id)"
                         version (format-flow jobs))))
    (setf (jenkins.api::dsl job) script)
    (jenkins.api:commit! job)))

(defun schedule-jobs (jobs)
  (let+ (((&labels dependency-closure (job)
            (remove-duplicates
             (cons job (mappend #'dependency-closure
                                ;; TODO temp
                                (remove job (dependencies job)))))))
         ((&flet sort-jobs (jobs)
            (jenkins.project::sort-with-partial-order ; TODO
             (copy-list jobs)
             (lambda (left right)
               (member left (dependencies right))))))
         ((&labels find-components (jobs)
            (let+ ((nodes (make-hash-table))
                   ((&flet add-job (job)
                      (let ((component '()))
                        (dolist (upstream (intersection jobs (dependency-closure job)))
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
                   (let* ((component (first components))
                          (index (floor (length component) 2))
                          (left (find-components (subseq component 0 index)))
                          (right (find-components (subseq component index))))
                     `(:serial ,@(if (typep left '(cons (eql :serial)))
                                     (rest left)
                                     (list left))
                               ,@(if (typep right '(cons (eql :serial)))
                                     (rest right)
                                     (list right)))))))))))
    (find-components jobs)))

(defun configure-jobs (distribution)
  (let ((name    (value distribution :distribution-name))
        (prepare (or (ignore-errors (value distribution :prepare-hook/unix))
                     "# Nothing to do"))
        (finish  (or (ignore-errors (value distribution :finish-hook/unix))
                     "# Nothing to do")))
    (macrolet ((ensure-job ((kind name) &body body)
                 `(let ((job (jenkins.dsl:job (,kind ,name) ,@body)))
                    (if (jenkins.api:job? (jenkins.api:id job))
                        (setf (jenkins.api:job-config (jenkins.api:id job))
                              (jenkins.api::%data job))
                        (jenkins.api::make-job (jenkins.api:id job) (jenkins.api::%data job)))
                    (jenkins.api:enable! job))))

      ;; Create helper jobs
      (ensure-job ("project" (format nil "toolkit-prepare-~A" name))
                  (jenkins.api:builders
                   (jenkins.dsl::shell (:command prepare))))
      (ensure-job ("project" (format nil "toolkit-finish-~A" name))
                  (jenkins.api:builders
                   (jenkins.dsl::shell (:command finish))))

      ;; Create bluildflow job
      (ensure-job ('("com.cloudbees.plugins.flow.BuildFlow" . "build-flow-plugin@0.10")
                    (format nil "toolkit-buildflow-~A" name))))))

(defun configure-distribution (distribution)
  (let ((name (value distribution :distribution-name))
        (jobs (remove-if-not            ; TODO hack
               (lambda (job)
                 (equal (ignore-errors (value job :variant)) (name distribution)))
               (mappend (compose #'jobs #'implementation) (versions distribution)))))
    (log:trace "~@<Jobs in ~A: ~A~@:>" distribution jobs)
    (configure-jobs distribution)
    (configure-buildflow-job name (schedule-jobs jobs))))

(defun configure-distributions (distributions)
  (mapc #'configure-distribution distributions))

;;; Commandline options

(defun update-synopsis ()
  "Create and return a commandline option tree."
  (clon:make-synopsis
   ;; Basic usage and specific options.
   :postfix "(INPUT-SPEC)+"
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
                      "Progress display style."))
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
              (stropt :long-name     "base-uri"
                      :short-name    "b"
                      :argument-name "URI"
                      :default-value "http://localhost:8080"
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
                      "Delete previously automatically generated jobs when they are not re-created in this generation run."))
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

(defun report-error (condition)
  (ignore-errors
   (format *error-output* "~&~@<~A:~
                           ~@:_~2@T~<~A~:>~
                           ~:>~2%"
           (type-of condition) (list condition))))

(defun call-with-delayed-error-reporting (thunk
                                          &key
                                          debug?
                                          (report-function #'report-error))
  (let+ ((errors      '())
         (errors-lock (bt:make-lock))
         ((&flet collect-error (condition)
            (when debug?
              (terpri)
              (princ condition)
              (terpri)
              (sb-debug:print-backtrace))
            (bt:with-lock-held (errors-lock)
              (push condition errors))
            (continue))))
    ;; Execute THUNK collecting errors.
    (lparallel:task-handler-bind ((error #'collect-error))
      (handler-bind ((error #'collect-error))
        (funcall thunk)))

    ;; Report collected errors.
    (mapc (lambda (condition)
            (ignore-errors (funcall report-function condition)))
          errors)))

(defmacro with-delayed-error-reporting ((&key debug? report-function)
                                        &body body)
  `(call-with-delayed-error-reporting
    (lambda () ,@body)
    ,@(when debug? `(:debug? ,debug?))
    ,@(when report-function `(:report-function ,report-function))))

;;; Main

(defun main ()
  (log:config :thread :info)

  (update-synopsis)
  (clon:make-context)
  (when (clon:getopt :long-name "help")
    (clon:help)
    (uiop:quit))

  (let* ((jenkins.api:*base-url* (clon:getopt :long-name "base-uri"))
         (jenkins.api:*username* (clon:getopt :long-name "username"))
         (jenkins.api:*password* (or (clon:getopt :long-name "password")
                                     (clon:getopt :long-name "api-token")))
         (delete-other?          (clon:getopt :long-name "delete-other"))
         (*print-right-margin*   (if-let ((value (sb-posix:getenv "COLUMNS")))
                                   (parse-integer value)
                                   200))

         (templates (sort (iter (for spec next (clon:getopt :long-name "template"))
                                (while spec)
                                (if-let ((matches (collect-inputs
                                                   (parse-namestring spec))))
                                  (appending matches)
                                  (warn "~@<Template pattern ~S did not match anything.~@:>"
                                        spec)))
                          #'string< :key #'pathname-name))
         (projects (iter (for spec in (clon:remainder))
                         (if-let ((matches (collect-inputs (parse-namestring spec))))
                           (appending matches)
                           (warn "~@<Project pattern ~S did not match anything.~@:>"
                                 spec))))
         (distributions (iter (for spec next (clon:getopt :long-name "distribution"))
                              (while spec)
                              (if-let ((matches (collect-inputs
                                                 (parse-namestring spec))))
                                (appending matches)
                                (warn "~@<Distribution pattern ~S did not match anything.~@:>"
                                      spec)))))

    (setf lparallel:*kernel* (lparallel:make-kernel 8))

    (with-delayed-error-reporting (:debug? (clon:getopt :long-name "debug"))
      (with-trivial-progress (:jobs)

        (let* ((templates     (load-templates templates))
               (specs         (load-projects projects))
               (distributions (tag-project-versions
                               (load-distributions distributions)))
               (projects      (instantiate-projects specs distributions))
               (jobs/spec     (flatten (deploy-projects projects)))
               (jobs          (mappend #'implementations jobs/spec)))
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
            (configure-distributions distributions))

          (enable-jobs jobs))))))
