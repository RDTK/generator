;;;; schema.lisp --- Schema for non-aspect variables.
;;;;
;;;; Copyright (C) 2014-2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:build-generator.model)

;;; Utilities

(defun dependency-specification? (thing)
  (when (proper-list-p thing)
    (or (typep thing '(cons (cons keyword string) null))
        (let ((nature?  nil)
              (target?  nil)
              (version? nil))
          (and (loop :for entry :in thing
                     :always (and (typep entry '(cons keyword string))
                                  (let ((key (car entry)))
                                    (case key
                                      (:nature  (unless nature?  (setf nature?  t)))
                                      (:target  (unless target?  (setf target?  t)))
                                      (:version (unless version? (setf version? t)))))))
               (and nature? target?))))))

(defun platform-dependency-variables? (variables)
  (if-let ((provides (assoc-value variables :extra-provides)))
    (every-dependency provides)
    t))

(defun platform-dependency-specification? (thing)
  (when (proper-list-p thing)
    (let ((name?      nil)
          (variables? nil))
      (and (loop :for entry :in thing
                 :always (typecase entry
                           ((cons (eql :name) string)
                            (unless name? (setf name? t)))
                           ((cons (eql :variables) list)
                            (and (unless variables? (setf variables? t))
                                 (platform-dependency-variables? (cdr entry))))))
           name?))))

(deftype dependency ()
  '(or (cons string (cons string (or null (cons string null)))) ; old [ NATURE, TARGET, [VERSION] ] style
       (satisfies dependency-specification?)))

(defun every-dependency (thing)
  (and (listp thing) (every (of-type 'dependency) thing)))

(deftype dependency-or-platform-dependency ()
  '(or dependency (satisfies platform-dependency-specification?)))

(defun every-dependency-or-platform-dependency (thing)
  (and (listp thing)
       (every (of-type 'dependency-or-platform-dependency) thing)))

;;;

(var:define-variable :distribution-name string
  :inheritance :outermost-only
  :documentation
  "Name of the distribution.")

;;; General variables

(var:define-variable :recipe.maintainer (or string (var:list-of string))
  :inheritance nil
  :documentation
  "Names and Email address of the maintainer(s) of the recipe.

   Either a string or a list of strings. In both cases, each
   individual person should be written in one of the following forms:

     NAME <EMAIL>
     NAME
     <EMAIL>

   .")

(var:define-variable :description string
  :inheritance nil
  :documentation
  "A short description of the project, distribution, etc.")

(var:define-variable :keywords (var:list-of string)
  :inheritance nil
  :aggregation :histogram
  :documentation
  "A list of keywords characterizing the project, distribution, etc.")

(var:define-variable :licenses (var:list-of string)
  :inheritance nil
  :aggregation :histogram
  :documentation
  "A list of names of licenses (usually one) associated to the project.

   It is not currently specified whether multiple licenses apply to
   different parts of the project or constitute alternatives from
   which a user can choose.")

(var:define-variable :programming-languages (var:list-of string)
  :inheritance nil
  :aggregation :histogram
  :documentation
  "A list of programming languages used in the project.")

(var:define-variable :platform-provides
    (var:list-of dependency-or-platform-dependency)
  :documentation
  "A list of things provided by the operating system or otherwise
   available \"a priori\".

   Entries are of one of the forms

     NATURE: TARGET

     nature: NATURE
     target: NAME

     nature: NATURE
     target: NAME
     version: VERSION

   where

   * NATURE is the build system (or dependency management system)
     within which the requirement originates such as \"cmake\",
     \"maven\", \"pkg-config\"

   * NAME is the name of the required artifact within the namespace
     designated by NATURE.

   * The optional VERSION specifies the provided version of the
     artifact.

   Example:

     nature: program
     target: sbin/spread
     version: 4.1

   Version 4.1 of the program installed under the name \"sbin/spread\"
   is provided by the platform.")

;;; Project variables

(var:define-variable :scm (or (eql :archive) (eql :git) (eql :svn) (eql :mercurial)
                              null string)
  :documentation
  "Forces the use a particular source code management system in the
   generator's analysis and generated Jenkins jobs.

   Common values are \"archive\", \"git\", \"svn\" and \"mercurial\".

   The value false can be used to overwrite and disable an inherited
   value.")

(var:define-variable :scm.credentials (or null string)
  :documentation
  "A reference to an entry in Jenkins' global credentials store that
   should be used for authentication by Jenkins when accessing the
   repository of this project.

   The specified string has to match the value of the \"description\"
   field of an existing entry in Jenkins' global credentials
   store.

   The following rules apply:

   * Projects with scm.credentials entries must specify private
     access.

   * Distributions with one or more private projects must specify
     private access.")

(var:define-variable :scm.username string
  :documentation
  "The username that should be used for accessing the source code
   management system of the project.

   Used in the generator's analysis and generated Jenkins jobs.")

(var:define-variable :scm.password (or null string)
  :documentation
  "The password that should be used for accessing the source code
   management system of the project.

   Used in the generator's analysis and generated Jenkins jobs.

   SECURITY NOTE: The given password will be visible in generated
   Jenkins jobs and build logs, even for Jenkins users with few
   permissions.")

(var:define-variable :scm.history-limit positive-integer
  :documentation
  "Do not use.")

(var:define-variable :repository string
  :documentation
  "The URL of the source repository, download location or filesystem
   location for the project.

   If the given URL contains the words \"git\", \"svn\", etc. the SCM
   kind is guessed automatically.")

(var:define-variable :branches (var:list-of string)
  :documentation
  "A list of names of branch available in the project repository.")

(var:define-variable :tags (var:list-of string)
  :documentation
  "A list of names of tags available in the project repository.")

(var:define-variable :branch string
  :documentation
  "The name of a specific branch to process.")

(var:define-variable :tag string
  :documentation
  "The name of a specific tag to process.")

(var:define-variable :directory string
  :documentation
  "Directory within a subversion repository.

   This variable is intended for subversion repositories that do not
   conform to the standard layout and thus cannot use the branch and
   tag variables.")

(var:define-variable :commit string
  :documentation
  "A specific commit to process.")

(var:define-variable :sub-directory string
  :documentation
  "The name of a sub-directory within the project repository in which
   the project should be considered to reside.

   The need for using this option arises when a single repository
   contains more than one projects and individual projects reside in
   sub-directories. The generator's analysis as well as Jenkins jobs
   will only act on the sub-directory and treat it as if it were the
   sole content of the repository.")

(var:define-variable :natures (var:list-of string)
  :inheritance nil
  :aggregation :histogram
  :documentation
  "A list of project natures the generator should consider when
   analyzing the source of the project.

   Common values are (combinations of) \"cmake\", \"setuptools\",
   \"maven\", \"ros-package\" and \"asdf\".")

(var:define-variable :extra-requires (var:list-of dependency)
  :documentation
  "A list of additional (i.e. not covered by automatic analysis)
   requirements.

   Entries are of one of the following forms

     NATURE: TARGET

     nature: NATURE
     target: TARGET

     nature: NATURE
     target: TARGET
     version: MIN-VERSION

   where

   * NATURE is the build system (or dependency management system)
     within which the requirement originates such as \"cmake\",
     \"maven\", \"pkg-config\"

   * TARGET is the name of the required artifact within the namespace
     designated by NATURE.

   * The optional MIN-VERSION specifies the minimum version of the
     artifact satisfying the requirement.

   Example:

     nature: program
     target: spread
     version: '4.1'

   Version 4.1 of the program installed under the name \"sbin/spread\"
   in the current installation prefix is required.")

(var:define-variable :extra-provides (var:list-of dependency)
  :documentation
  "A list of additional (i.e. not covered by automatic analysis)
   provides.

   Entries are of one of the following forms

     NATURE: TARGET

     nature: NATURE
     target: TARGET

     nature: NATURE
     target: TARGET
     version: VERSION

   where

   * NATURE is the build system (or dependency management system) by
     which the artifact can be used such as \"cmake\", \"maven\",
     \"pkg-config\"

   * TARGET is the name of the provided artifact within the namespace
     designated by NATURE.

   * The optional VERSION specifies the version of the provided
     artifact.

   Example:

     nature: program
     target: spread
     version: '4.1'

   Version 4.1 of the program installed under the name \"sbin/spread\"
   in the current installation prefix is provided.")

;;; Project and distribution variables

(var:define-variable :access (or (eql :private) (eql :public))
  :inheritance nil
  :documentation
  "Specifies access restrictions of the project or distribution. If present,
   has to have one of the values \"private\" and \"public\". Omitting the
   variable is equivalent to \"public\".

   Distributions with one or more private projects must specify
   private access.")

(var:define-variable :platform-requires list
  :inheritance nil
  :aggregation :merge
  :documentation
  "An object which can contain entries of the following forms

     OPERATING-SYSTEM-TYPE:
       packages:
       - PACKAGE₁
       - PACKAGE₂
       ⋮

   A list of packages that have to be installed when building on an
   operating system of type OPERATING-SYSTEM-TYPE, independent of
   the operating system version.

   Example values for OPERATING-SYSTEM-TYPE: \"ubuntu\", \"debian\".

     OPERATING-SYSTEM-TYPE:
       OPERATING-SYSTEM-VERSION:
         packages
         - PACKAGE₁
         - PACKAGE₂
         ⋮

   A list of packages that have to be installed when building on an
   operating system of type OPERATING-SYSTEM-TYPE with version
   OPERATING-SYSTEM-VERSION. Merged with version-independent
   package-requirements for OPERATING-SYSTEM-TYPE.

   Example values: for OPERATING-SYSTEM-VERSION when
   OPERATING-SYSTEM-TYPE is \"ubuntu\": \"precise\", \"trusty\".

     OPERATING-SYSTEM-TYPE:
       OPERATING-SYSTEM-VERSION:
         ARCHITECTURE:
           packages:
           - PACKAGE₁
           - PACKAGE₂
           ⋮

   A list of packages that have to be installed when building on a
   ARCHITECTURE machine with an operating system of type
   OPERATING-SYSTEM-TYPE with version OPERATING-SYSTEM-VERSION.
   Merged with the previous two entries.

   Example values: \"i686\", \"x86_64\".")

;;; Job variables

(var:define-variable :build-job-name string
  :documentation
  "Name of the Jenkins job that should be generated.")

(var:define-variable :kind (or string (cons string (cons string null)))
  :documentation
  "The kind of the Jenkins job that should be generated.

   Typically a string

     \"KIND\"

   where KIND is \"project\" or \"matrix\".")

(var:define-variable :build-job.disabled? (or boolean (eql :force-disabled))
  :documentation
  "Should the generated build job be disabled?

   false

     Enable newly created jobs. Leave existing jobs alone.

   true

     Disable newly created jobs. Leave existing jobs alone.

   \"force-disabled\"

     Disable newly created as well as existing jobs.")

(var:define-variable :build-job.orchestrate? boolean
  :documentation
  "Should the generated build job be managed by orchestration jobs?")

(var:define-variable :dependency-job-name string
  :documentation
  "Internal.")

(var:define-variable :dependencies.mode (or (eql :direct) (eql :minimal) (eql :none))
  :documentation
  "Controls up/downstream relations between generated jobs.

   \"direct\"

     An up/downstream relation should be established between generated
     jobs iff there is a direct dependency between the corresponding
     projects.

   \"minimal\"

     An up/downstream relation should be established between generated
     jobs U(pstream) and D(ownstream) iff there is a direct dependency
     between the corresponding projects and no transitive downstream
     job of U is also an upstream job of D.

   \"none\"

     No up/downstream relations should be established between
     generated jobs.")

(var:define-variable :dependencies.required-upstream-result
    (or (eql :success) (eql :unstable) (eql :any))
  :documentation
  "Controls the required status of upstream jobs in order to trigger
   builds of downstream jobs.

   \"success\" (default)

     All upstream jobs must have successful builds.

   \"unstable\"

     All upstream jobs must have unstable (e.g. with test failures) or
     successful builds.

   \"any\"

     The build of the downstream job can start irregardless of the
     status of the upstream jobs.")

(var:define-variable :dependencies.required-result
    (or (eql :success) (eql :unstable) (eql :any))
  :documentation
  "Specifies the status of jobs required to trigger downstream
   jobs.

   This can be used to relax the threshold established via
   dependencies.required-upstream-result in cases in which a
   known-unreliable upstream should still trigger downstream jobs.
   Note that marking the upstream job avoids the need to adapt the
   threshold in all downstream jobs.

   \"success\" (default)

     Only trigger downstream jobs if the build of the upstream job was
     successful (unless some downstream jobs specify a less strict
     threshold).

   \"unstable\"

     Trigger downstream jobs if the build result of the upstream job
     was \"success\" or \"unstable\" (unless some downstream jobs
     specify a less strict threshold).

   \"any\"

     Trigger downstream jobs irregardless of the build result of the
     upstream job.")

(var:define-variable :upstream-dir string
  :documentation
  "Directory in which artifacts copied from upstream jobs should be
   placed.")

;;; Orchestration variables

(var:define-variable :jobs.list list
  :documentation
  "A list of names for all Jenkins jobs.

   Does not include \"orchestration\" jobs.")

(var:define-variable :jobs.dependencies list
  :documentation
  "An association between up- and downstream build jobs.")

(var:define-variable :jobs.dependencies/groovy string
  :documentation
  "jobs.dependencies, formatted as a Groovy script string.

   The association is represented as a map of the form

     [
       \"UPSTREAM-NAME\": [ \"DOWNSTREAM-NAME\", ... ],
       ...
     ]

   .")

;;; View variables

(var:define-variable :view.create? boolean
  :documentation
  "If true, create an associated view for each created distribution.")

(var:define-variable :view.name string
  :documentation
  "The name of the associated view for the current distribution.

   Has no effect if the value of view.create? is not true.")

(var:define-variable :view.columns (var:list-of string)
  :documentation
  "Column class names for the associated view for the current distribution.")
