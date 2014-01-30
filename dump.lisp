(ql:quickload '(:rosetta :jenkins.project :cl-dot :cl-who :cxml-rpc
                :terminal.ansi-escapes :user-interface.progress :lparallel :com.dvlsoft.clon
                :swank))
(require :sb-sprof)
(require :sb-posix)
(require :sb-grovel)
(require :sb-concurrency)
(load (merge-pathnames "src/project/graphviz.lisp" *load-pathname*))
(load (merge-pathnames "drupal-xmlrpc.lisp" *load-pathname*))

(cl:in-package #:jenkins.project)

(sb-ext:add-package-local-nickname '#:clon '#:com.dvlsoft.clon)

;;; Progress stuff

#+no (defmethod jenkins.analysis:analyze :before ((source t) (schema t) &key)
  (progress :analyze/branch nil "Analyzing ~A branch ~A" schema source))

(defmethod deploy :around ((thing project))
  (with-sequence-progress (:deploy/version (versions thing))
    (call-next-method)))

(defmethod deploy :around ((thing version))
  (progress :deploy/version nil "~A" thing)
  (with-sequence-progress (:deploy/job (jobs thing))
    (call-next-method)))

(defmethod deploy :before ((thing job))
  (progress :deploy/job nil "~A" thing))

;;; Input

(defun load-templates (files)
  (with-sequence-progress (:templates files)
    (dolist (file files)
      (progress "~S" file)
      (restart-case
          (load-template/json file)
        (continue (&optional condition)
          :report (lambda (stream)
                    (format stream "~@<Skip template specification ~
                                    ~S.~@:>"
                            file))
          (declare (ignore condition)))))))

(defun analyze-project (project)
  (let+ (((&labels+ do-branch ((branch . info))
            (ensure-directories-exist "info/")
            (with-output-to-file (stream (format nil "info/~A-~A.txt" (name project) branch)
                                         :if-does-not-exist :create
                                         :if-exists         :supersede)
              (write info :stream stream))

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
                                         (%direct-variables version)
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

    (mapc #'do-branch (jenkins.analysis:analyze
                        (puri:uri (lookup project :repository)) :auto
                        :scm           (ignore-errors (lookup project :scm))
                        :username      (ignore-errors (lookup project :scm.username))
                        :password      (ignore-errors (lookup project :scm.password))
                        :branches      (ignore-errors (lookup project :branches))
                        :tags          (ignore-errors (lookup project :tags))
                        :sub-directory (ignore-errors (lookup project :sub-directory)))))

  project)

(defun load-projects (files)
  "TODO(jmoringe): document"
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
      :parts most-positive-fixnum files))


    #+no (iter (for file in files)
          (progress "~A" file)
          (restart-case
              (let ((project (load-project-spec/json file)))
                (collect
                    (setf (find-project (name project))
                          (analyze-project project))))
            (continue (&optional condition)
              :report (lambda (stream)
                        (format stream "~@<Skip ~S.~@:>" file))
              (declare (ignore condition)))))))

;; Deployment

(defun instantiate-projects (specs)
  "TODO(jmoringe): document"
  (let ((projects (mapcar #'instantiate specs)))
    (iter (for project in projects)
          (for spec    in specs)
          (when project
            (add-dependencies! project spec)
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
                                (format stream "~@<Skip ~S.~@:>" project))
                      (declare (ignore condition))))))))

    (with-sequence-progress (:deploy/dependencies jobs)
      (iter (for job in jobs)
            (progress "~A" job)
            (restart-case
                (deploy-dependencies job)
              (continue (&optional condition)
                :report (lambda (stream)
                          (format stream "~@<Skip ~S.~@:>" job))
                (declare (ignore condition))))))

    jobs))

(defun enable-jobs (jobs)
  (with-sequence-progress (:enable jobs)
    (iter (for job in jobs)
          (progress "~S" job)
          (restart-case
              (jenkins.api:enable! (jenkins.api:job (id job)))
            (continue (&optional condition)
              (declare (ignore condition)))))))

(defun generated-jobs ()
  "TODO(jmoringe): document"
  (remove-if-not
   (lambda (job)
     (search "automatically generated" (jenkins.api:description job)))
   (jenkins.api:all-jobs)))

;;; Toolkit specific stuff

(defun find-toolkit-versions (jobs)

  (let ((versions (make-hash-table :test #'equal)))
    (mapc (lambda (job)
            (when-let ((version (ignore-errors
                                 (value job :toolkit.version))))
              (push job (gethash version versions '()))))
          jobs)
    versions))

(defun format-flow (jobs)
  (etypecase jobs
    ((cons (eql :parallel))
     (format nil "parallel (~%~2@T~<~@;~{{~A}~^,~%~}~:>~%)"
             (list (mapcar #'format-flow (rest jobs)))))
    ((cons (eql :serial))
     (format nil "~%~2@T~<~@;~{~A~^~%~}~:>~%"
             (list (mapcar #'format-flow (rest jobs)))))
    (t
     (format nil "~:[~:;ignore(FAILURE) {~%~2@T~]build(~S)~2:*~:[~:;~%}~]"
             nil jobs))))

(defun configure-buildflow-job (version jobs #+no job-components
                                             &key (ignore-failures? t))
  (let+ ((name   (format nil "toolkit-buildflow-~A" version))
         (job    (first (all-jobs name)))
         ((&labels format-flow (jobs)
            (etypecase jobs
              ((cons (eql :parallel))
               (format nil "parallel (~%~2@T~<~@;~{{~A}~^,~%~}~:>~%)"
                       (list (mapcar #'format-flow (rest jobs)))))
              ((cons (eql :serial))
               (format nil "~%~2@T~<~@;~{~A~^~%~}~:>~%"
                       (list (mapcar #'format-flow (rest jobs)))))
              (t
               (format nil "~:[~:;ignore(FAILURE) {~%~2@T~]build(~S)~2:*~:[~:;~%}~]"
                       ignore-failures? jobs)))))
         #+no (script (format nil "build(\"toolkit-cleanup-~A\")~2%~
                              parallel (~%~
                                ~{~2@T{~%~
                                  ~{~4@Tignore(FAILURE) {~%~
                                    ~6@Tbuild(~S)~%~
                                  ~4@T}~%~}~
                                ~2@T}~^,~%~}~
                              ~%)~2%~
                              build(\"toolkit-deploy-~2:*~A\")"
                         version
                         (mapcar
                          (lambda (component)
                            (mapcar (compose #'id #'implementation) component))
                          job-components)))
         (script (format nil "build(\"toolkit-cleanup-~A\")~2%~
                              ~A~2%~
                              build(\"toolkit-deploy-~2:*~A\")"
                         version
                         #+no (print `(:parallel
                            ,@(mapcar
                               (lambda (component)
                                 `(:serial ,@(mapcar (compose #'id #'implementation) component)))
                               job-components)))
                         (format-flow jobs))))
    (setf (jenkins.api::dsl job) script)
    (commit! job)))

(defun schedule-jobs (jobs)
  (let+ (((&labels dependency-closure (job)
            (remove-duplicates
             (cons job (mappend #'dependency-closure
                                ;; TODO temp
                                (remove job (dependencies job)))))))
         ((&flet sort-jobs (jobs)
            (sort-with-partial-order (copy-list jobs)
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
                   (id (implementation (first (first components)))))
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

(defun configure-orchestration-jobs (version jobs)
  (let+ (((&labels dependency-closure (job)
            (remove-duplicates
             (cons job (mappend #'dependency-closure
                                ;; TODO temp
                                (remove job (dependencies job)))))))
         (nodes (make-hash-table))
         ((&flet add-job (job)
            (let ((component '()))
              (dolist (upstream (dependency-closure job))
                (unionf component (gethash upstream nodes (list upstream)))
                (dolist (member component)
                  (setf (gethash member nodes) component))))))
         ((&flet sort-jobs (jobs)
            (sort-with-partial-order (copy-list jobs)
                                     (lambda (left right)
                                       (member left (dependencies right))))))
         (components (progn
                       (mapc #'add-job jobs)
                       (remove-duplicates (hash-table-values nodes)))))
    (configure-buildflow-job version (mapcar #'sort-jobs components))))

(defun configure-toolkit-versions (jobs)
  (let ((versions (find-toolkit-versions jobs)))
    (iter (for (version jobs) in-hashtable versions)
          #+no (configure-orchestration-jobs version jobs)
          (configure-buildflow-job version (schedule-jobs jobs)))))

;;; Commandline interface

(defun collect-inputs (spec)
  "TODO(jmoringe): document"
  (cond
    ((wild-pathname-p spec)
     (directory spec))
    ((pathnamep spec)
     (list spec))
    (t
     (error "~@<Invalid input specification: ~S.~@:>" spec))))

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
                      "Load a template. This option can be supplied multiple times.")
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
                      :short-name    "d"
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

(defun main ()
  (update-synopsis)
  (clon:make-context)
  (when (clon:getopt :long-name "help")
    (clon:help)
    (sb-ext:exit))

  (when (clon:getopt :long-name "swank")
    (ignore-errors
     (delete-file "/tmp/swank.port"))
    (swank:start-server "/tmp/swank.port"))

  (let* ((jenkins.api:*base-url* (clon:getopt :long-name "base-uri"))
         (jenkins.api:*username* (clon:getopt :long-name "username"))
         (jenkins.api:*password* (or (clon:getopt :long-name "password")
                                      (clon:getopt :long-name "api-token")))
         (delete-other?          (clon:getopt :long-name "delete-other"))
         (errors                 '())
         (errors-lock            (bt:make-lock))
         (*print-right-margin*   (if-let ((value (sb-posix:getenv "COLUMNS")))
                                   (parse-integer value)
                                   200)))
    (user-interface.progress:with-progress-report (*standard-output* :style (ecase (clon:getopt :long-name "progress-style")
                                                                              (:cmake    (make-instance 'user-interface.progress::cmake))
                                                                              (:vertical (make-instance 'user-interface.progress::vertical)))
                                                                     :update t #+no 1/10)
      (setf lparallel:*kernel*
            (lparallel:make-kernel
             8 :context (let ((handle user-interface.progress:*handle-condition-function*))
                          (lambda (thunk)
                            (if handle
                                (handler-bind ((progress-condition handle)) (funcall thunk))
                                (funcall thunk))))))
      (with-trivial-progress (:jobs)

        (let ((templates (sort (iter (for spec next (clon:getopt :long-name "template"))
                                     (while spec)
                                     (if-let ((matches (collect-inputs
                                                        (parse-namestring spec))))
                                       (appending matches)
                                       (warn "~@<Template pattern ~S did not match anything.~@:>"
                                             spec)))
                               #'string< :key #'pathname-name))
              (projects (iter (for spec in (clon:remainder))
                              (if-let ((matches (collect-inputs
                                                 (parse-namestring spec))))
                                (appending matches)
                                (warn "~@<Project input ~S did not match anything.~@:>"
                                      spec)))))
          (lparallel:task-handler-bind
              ((error (lambda (condition)
                        (when nil
                          (terpri)
                          (princ condition)
                          (terpri)
                          (sb-debug:print-backtrace))
                        (bt:with-lock-held (errors-lock)
                          (push condition errors))
                        (continue))))
            (handler-bind
                ((error (lambda (condition)
                          (when nil
                            (terpri)
                            (princ condition)
                            (terpri)
                            (sb-debug:print-backtrace))
                          (bt:with-lock-held (errors-lock)
                            (push condition errors))
                          (continue))))
              (let* ((templates (load-templates templates))
                     (specs     (load-projects projects))
                     (projects  (instantiate-projects specs))
                     (jobs/spec (flatten (deploy-projects projects)))
                     (jobs      (mapcar #'implementation jobs/spec)))
                (declare (ignore templates))

                ;; Draw graphs
                (ensure-directories-exist "graphs/")
                (let+ (((&flet graph (kind roots name)
                          (cl-dot:dot-graph
                           (cl-dot:generate-graph-from-roots
                            kind roots '(:rankdir "LR"))
                           (namestring
                            (merge-pathnames
                             (format nil "~A-~(~A~).svg" name kind)
                             "graphs/dummy.svg"))
                           :format :svg))))

                  (with-sequence-progress (:graphs/project projects)
                    (iter (for project in projects)
                          (progress "~A" project)
                          (graph :jenkins.project (list project) (name project))))

                  (with-sequence-progress (:graphs/dependencies projects)
                    (iter (for project in projects)
                          (progress "~A" project)
                          (graph :jenkins.dependencies (versions project) (name project)))))

                ;; Delete automatically generated jobs found on the
                ;; server for which no counterpart exists among the newly
                ;; generated jobs. This is necessary to get rid of
                ;; leftover jobs when projects (or project versions) are
                ;; deleted or renamed.
                (when delete-other?
                  (let ((other-jobs (set-difference (generated-jobs) jobs
                                                    :key #'id :test #'string=)))
                    (with-sequence-progress (:delete-other other-jobs)
                      (mapc (progressing #'jenkins.api::delete-job :delete-other)
                            other-jobs))))

                ;; TODO explain
                (with-trivial-progress (:orchestration "Configuring orchestration jobs")
                  (configure-toolkit-versions jobs/spec))

                (enable-jobs jobs)))))))

    ;; Report errors.
    (mapc (lambda (error)
            (ignore-errors
             (format *error-output* "~&~A:~&~A~2%"
                     (type-of error) error)))
          errors)))

#+no (trace jenkins.api:relate)

#+no (push (lambda () (setf lparallel:*kernel* (lparallel:make-kernel 4)))
      sb-ext:*init-hooks*)

#+no (push (lambda () (lparallel:end-kernel :wait t))
      sb-ext:*save-hooks*)

#+no (trace add-dependencies!)
#+no (trace deploy)

#+no (trace aspects applicable-aspects)

#+no (trace sort-with-partial-order)

#+no (trace jenkins.project::conditions jenkins.project::instantiate? jenkins.project::instantiate)
#+no (trace jenkins.project::value)

#+no (trace drakma:http-request)

#+no (trace variables)

#+no (trace find-toolkit-versions configure-buildflow-job configure-orchestration-jobs configure-toolkit-versions)

#+no (trace jenkins.analysis:analyze)

#+no (trace "JENKINS.ANALYSIS")

#+no (trace rs.f::normalize-name)

(log:config :thread :info)

(sb-ext:save-lisp-and-die "projects"
                          :toplevel             'main
                          :executable           t
                          :save-runtime-options t)
