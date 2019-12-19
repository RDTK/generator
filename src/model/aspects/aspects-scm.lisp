;;;; aspects-scm.lisp --- Definitions of SCM-related aspects
;;;;
;;;; Copyright (C) 2012-2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:build-generator.model.aspects)

(defun make-focus-sub-directory-command (sub-directory &key exclude)
  (let+ ((sub-directory (uiop:ensure-directory-pathname sub-directory))
         ((&whole components first &rest &ign)
          (rest (pathname-directory sub-directory))))
    (format nil "~A~@
                 ~@
                 ~A"
            (make-remove-directory-contents/unix
             :exclude (list* first exclude))
            (make-move-stuff-upwards/unix components))))

(define-aspect (archive) (builder-defining-mixin)
    ((url                                  :type string
      :documentation
      "URL from which the archive should be downloaded.

       HTTP and HTTPS are supported.")
     ((filename nil)                       :type string
      :documentation
      "Name of the file in which the downloaded archive should be
       stored.")
     (((:sub-directory sub-directory) nil) :type string))
  "Adds a build step that downloads a source archive from URL as FILENAME.

   This may be useful when a SCM repository is not available but
   source archives are."
  ;; In case we are updating an existing job, remove any repository
  ;; configuration.
  (setf (jenkins.api:repository job) (make-instance 'jenkins.api:scm/null))

  ;; Generate archive download and extraction as a shell builder.
  (let* ((url/parsed (puri:uri url))
         (archive    (or filename (lastcar (puri:uri-parsed-path url/parsed))))
         (command    (format nil "# Clean workspace.~@
                                  ~A~@
                                  ~@
                                  # Unpack archive.~@
                                  wget --no-verbose \"~A\" --output-document=\"~A\"~@
                                  unp -U \"~:*~A\"~@
                                  rm \"~:*~A\"~@
                                  directory=$(find . -mindepth 1 -maxdepth 1)~@
                                  ~@
                                  ~A"
                             (make-remove-directory-contents/unix)
                             url archive
                             (make-move-stuff-upwards/unix
                              (list* "${directory}"
                                     (when sub-directory
                                       (rest (pathname-directory
                                              (uiop:ensure-directory-pathname
                                               sub-directory)))))))))
    (push (constraint! (build ((:before t)))
            (make-instance 'jenkins.api:builder/shell :command command))
          (jenkins.api:builders job))))

(define-aspect (git :job-var    job
                    :aspect-var aspect
                    :plugins    ("git"))
    (builder-defining-mixin)
    ((url                                   :type string
      :documentation
      "URL of the remote git repository from which the project source
       should be cloned.")
     ((username                       nil)  :type string
      :documentation
      "Username that should be used when cloning the remote repository.

       Should rarely be necessary since the credentials parameter can
       is often more appropriate.")
     ((password                       nil)  :type string
      :documentation
      "Password to use when cloning the remote repository.

       HUGE SECURITY RISK. Use the credentials parameter instead.")
     ((credentials                    nil)  :type string
      :documentation
      "Name of an entry in Jenkins' global credentials store that
       should be use for authenticating against the remote repository
       server.")
     (branches                              :type (var:list-of string)
      :documentation
      "List of names of branches in the git repository that should be
       checked out.")
     ((local-branch                   nil)  :type string)
     ((clone-timeout                  nil)  :type positive-integer
      :documentation
      "Timeout for the git clone operation in minutes.")
     ((wipe-out-workspace?            nil)  :type boolean
      :documentation
      "Controls whether the entire workspace, including previously
       cloned git repositories, should be deleted before the build,
       thus forcing a fresh clone and new build from scratch.

       Not recommended unless a project does not build without it.")
     ((clean-before-checkout?         t)    :type boolean
      :documentation
      "Delete all files in the workspace that are not under version
       control before building.

       Can be used to force building from scratch without deleting the
       cloned repository, thus enabling incremental updates instead of
       fresh clones for every build.")
     ((checkout-submodules?           nil)  :type boolean
      :documentation
      "Should sub-modules be cloned and checked out?

       This is sometimes called a \"recursive clone\".")
     ((shallow?                       nil)  :type boolean
       :documentation
      "Should a shallow clone, that is not fetching only recent and
       not the full repository history, be performed?")
     (((:sub-directory sub-directory) nil)  :type string))
  "Configures a GIT repository in the generated job.

   If USERNAME and PASSWORD are supplied, the supplied values may show
   up in the configuration and/or build logs of the generated
   job. Handle with care.

   If CREDENTIALS is supplied, a corresponding entry has to be created
   in the global Jenkins credentials configuration."
  ;; Configure GIT scm plugin.
  (let* ((url/parsed  (puri:uri url))
         (credentials (or credentials
                          (unless (model:check-access aspect :public)
                            (puri:uri-host url/parsed)))))
    (setf (jenkins.api:repository job)
          (make-instance 'jenkins.api:scm/git
                         :url                    (build-generator.analysis::format-git-url
                                                  url/parsed username password)
                         :credentials            credentials
                         :branches               branches
                         :clone-timeout          clone-timeout
                         :wipe-out-workspace?    wipe-out-workspace?
                         :clean-before-checkout? clean-before-checkout?
                         :checkout-submodules?   checkout-submodules?
                         :shallow?               shallow?
                         :local-branch           local-branch
                         :internal-tag?          nil)))

  ;; If a specific sub-directory of the repository has been requested,
  ;; move the contents of that sub-directory to the top-level
  ;; workspace directory before proceeding.
  (when sub-directory
    (push (constraint! (build ((:before t)))
            (make-instance 'jenkins.api:builder/shell
                           :command (make-focus-sub-directory-command
                                     sub-directory :exclude '(".git"))))
          (jenkins.api:builders job))))

(define-aspect (git-repository-browser
                :job-var     job
                :constraints ((:after aspect-git))
                :plugins     ("git"))
    ()
    (((kind (bail)) :type keyword)
     ((url  (bail)) :type string))
  "Configures the GIT-specific repository browser of the generated job "
  (let ((repository (jenkins.api:repository job)))
    (unless (typep repository 'jenkins.api:scm/git)
      (error "~@<Could not find git repository in ~A.~@:>" job))
    (setf (jenkins.api:browser-kind repository) kind
          (jenkins.api:browser-url  repository) url)))

(define-aspect (subversion :job-var    job
                           :aspect-var aspect
                           :plugins    ("subversion"))
    ()
    ((url                             :type string
     :documentation
     "URL from which the checkout should be performed including,
      tag/branch path and optionally sub-directory.")
     ((revision          nil)         :type string
      :documentation
      "A subversion revision that should be checked out instead of the
       newest revision of the specified path.")
     ((credentials       nil)         :type string
      :documentation
      "Name of an entry in Jenkins' global credentials store that
       should be use for authenticating against the remote repository
       server.")
     (local-dir                       :type string)
     ((checkout-strategy :fresh-copy) :type (or (eql :fresh-copy)
                                                (eql :update)
                                                (eql :emulate-fresh-copy))
      :documentation
      "The strategy to use when checking out from a remote repository.

       fresh-copy

         Always start from scratch and retrieve everything from the
         remote repository.

       update

         Keep a local checkout and update it by transferring
         differences from the remote repository.

       emulate-fresh-copy

         ?"))
  "Configures a Subversion repository in the generated job.

   If CREDENTIALS is supplied, a corresponding entry has to be created
   in the global Jenkins credentials configuration."
  (let* ((url/parsed   (puri:uri url))
         (url/parsed   (puri:copy-uri
                        url/parsed
                        :path (ppcre:regex-replace-all
                               "//+" (puri:uri-path url/parsed) "/")))
         (url/revision (format nil "~A~@[@~A~]" url/parsed revision))
         (credentials  (or credentials
                           (unless (model:check-access aspect :public)
                             (puri:uri-host url/parsed)))))
    (setf (jenkins.api:repository job)
          (make-instance 'jenkins.api:scm/svn
                         :url               url/revision
                         :credentials       credentials
                         :local-directory   local-dir
                         :checkout-strategy checkout-strategy))))

(define-aspect (mercurial :job-var    job
                          :aspect-var aspect
                          :plugins    ("mercurial"))
    (builder-defining-mixin)
    ((url                                  :type string
      :documentation
      "URL of the remote mercurial repository from which the project
       source should be cloned.")
     ((credentials                    nil) :type string
      :documentation
      "Name of an entry in Jenkins' global credentials store that
       should be use for authenticating against the remote repository
       server.")
     ((branch                         nil) :type string
      :documentation
      "Name of the branch in the mercurial repository that should be
       checked out.

       Mutually exclusive with the tag parameter.")
     ((tag                            nil) :type string
      :documentation
      "Name of the tag in the mercurial repository that should be
       checked out.

       Mutually exclusive with the branch parameter.")
     (clean?                                :type boolean
      :documentation
      "Controls whether the workspace is cleaned before each build.")
     (((:sub-directory sub-directory) nil) :type string))
  "Configures the Mercurial repository at URL for the generated job.

   If CREDENTIALS is supplied, a corresponding entry has to be created
   in the global Jenkins credentials configuration."
  ;; Configure mercurial scm plugin.
  (let* ((url/parsed   (puri:uri url))
         (credentials  (or credentials
                           (unless (model:check-access aspect :public)
                             (puri:uri-host url/parsed)))))
    (when (and branch tag)
      (error "~@<Cannot specify branch ~S and tag ~S at the same time.~@:>"
             branch tag))
    (setf (jenkins.api:repository job)
          (make-instance 'jenkins.api:scm/mercurial
                         :url           url
                         :credentials   credentials
                         :revision-type (cond
                                          (branch :branch)
                                          (tag    :tag))
                         :branch        (or branch tag)
                         :clean?        clean?)))

  ;; If a specific sub-directory of the repository has been requested,
  ;; move the contents of that sub-directory to the top-level
  ;; workspace directory before proceeding.
  (when sub-directory
    (push (constraint! (build ((:before t)))
            (make-instance 'jenkins.api:builder/shell
                           :command (make-focus-sub-directory-command
                                     sub-directory :exclude '(".hg"))))
          (jenkins.api:builders job))))

(define-aspect (trigger/scm) ()
    ((spec :type (or null string)
      :documentation
      "Describes the schedule Jenkins should employ for polling the
       repository.

       See Jenkins documentation for details."))
  "Configures the generated job such that it polls the SCM repository."
  (removef (jenkins.api:triggers job) 'jenkins.api:trigger/scm :key #'type-of)
  (when spec
    (push (make-instance 'jenkins.api:trigger/scm :spec spec)
          (jenkins.api:triggers job))))
