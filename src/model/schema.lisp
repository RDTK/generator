;;;; schema.lisp --- Schema for non-aspect variables.
;;;;
;;;; Copyright (C) 2014, 2015, 2016, 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.model)

;;; General variables

(define-variable :system-packages list
  "Obsolete. Do not use.")

(define-variable :platform-provides list
  "A list of things provided by the operating system or otherwise
   available \"a priori\".

   Entries are of one of the forms

     [ \"NATURE\", \"NAME\"
     [ \"NATURE\", \"NAME\", \"VERSION\" ]

   where

     NATURE is the build system (or dependency management system)
     within which the requirement originates such as \"cmake\",
     \"maven\", \"pkg-config\"

     NAME is the name of the required artifact within the namespace
     designated by NATURE.

     The optional VERSION specifies the provided version of the
     artifact.

   Examples:

     [ \"program\", \"sbin/spread\", \"4.1\" ]

       Version 4.1 of the program installed under the name
       \"sbin/spread\" is provided by the platform.")

;;; Project variables

(define-variable :scm string
  "Forces the use a particular source code management system in the
   generator's analysis and generated Jenkins jobs.

   Common values are \"archive\", \"git\", \"svn\" and \"mercurial\".")

(define-variable :scm.credentials string
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

(define-variable :scm.username string
  "The username that should be used for accessing the source code
   management system of the project.

   Used in the generator's analysis and generated Jenkins jobs.")

(define-variable :scm.password string
  "The password that should be used for accessing the source code
   management system of the project.

   Used in the generator's analysis and generated Jenkins jobs.

   SECURITY NOTE: The given password will be visible in generated
   Jenkins jobs and build logs, even for Jenkins users with few
   permissions.")

(define-variable :scm.history-limit string
  "Do not use.")

(define-variable :repository string
  "The URL of the source repository, download location or filesystem
   location for the project.

   If the given URL contains the words \"git\", \"svn\", etc. the SCM
   kind is guessed automatically.")

(define-variable :branches list
  "A list of names of branch available in the project repository.")

(define-variable :tags list
  "A list of names of tags available in the project repository.")

(define-variable :sub-directory string
  "The name of a sub-directory within the project repository in which
   the project should be considered to reside.

   The need for using this option arises when a single repository
   contains more than one projects and individual projects reside in
   sub-directories. The generator's analysis as well as Jenkins jobs
   will only act on the sub-directory and treat it as if it were the
   sole content of the repository.")

(define-variable :description string
  "Textual description of the project or distribution.")
(define-variable :description.header string)
(define-variable :description.footer string)

(define-variable :natures list
  "A list of project natures the generator should consider when
   analyzing the source of the project.

   Common values are (combinations of) \"cmake\", \"setuptools\",
   \"maven\", \"ros-package\" and \"asdf\".")

(define-variable :extra-requires list
  "A list of additional (i.e. not covered by automatic analysis)
   requirements.

   Entries are of one of the following forms

     [ \"NATURE\", \"NAME\" ]
     [ \"NATURE\", \"NAME\", \"MIN-VERSION\" ]

   where

     NATURE is the build system (or dependency management system)
     within which the requirement originates such as \"cmake\",
     \"maven\", \"pkg-config\"

     NAME is the name of the required artifact within the namespace
     designated by NATURE.

     The optional MIN-VERSION specifies the minimum version of the
     artifact satisfying the requirement.

   Examples:

     [ \"program\", \"sbin/spread\", \"4.1\" ]

       Version 4.1 of the program installed under the name
       \"sbin/spread\" in the current installation prefix is
       required.")

(define-variable :extra-provides list
  "A list of additional (i.e. not covered by automatic analysis)
   provides.

   Entries are of one of the following forms

     [ \"NATURE\", \"NAME\" ]
     [ \"NATURE\", \"NAME\", \"VERSION\" ]

   where

     NATURE is the build system (or dependency management system) by
     which the artifact can be used such as \"cmake\", \"maven\",
     \"pkg-config\"

     NAME is the name of the provided artifact within the namespace
     designated by NATURE.

     The optional VERSION specifies the version of the provided
     artifact.

   Examples:

     [ \"program\", \"sbin/spread\", \"4.1\" ]

       Version 4.1 of the program installed under the name
       \"sbin/spread\" in the current installation prefix is
       provided.")

;;; Project and distribution variables

(define-variable :access string
  "Specifies access restrictions of the project or distribution. If present,
   has to have one of the values \"private\" and \"public\". Omitting the
   variable is equivalent to \"public\".

   Distributions with one or more private projects must specify
   private access.")

(define-variable :platform-requires list
  "An object which can contain entries of the following forms

     \"OPERATING-SYSTEM-TYPE\": {
         \"packages\": [ \"PACKAGE₁\", \"PACKAGE₂\", … ]
     }

   A list of packages that have to be installed when building on an
   operating system of type OPERATING-SYSTEM-TYPE, independent of
   the operating system version.

   Example values for OPERATING-SYSTEM-TYPE: \"ubuntu\", \"debian\".

     \"OPERATING-SYSTEM-TYPE\": {
         \"OPERATING-SYSTEM-VERSION\": {
             \"packages\": [ \"PACKAGE₁\", \"PACKAGE₂\", … ]
         }
     }

   A list of packages that have to be installed when building on an
   operating system of type *OPERATING-SYSTEM-TYPE* with version
   *OPERATING-SYSTEM-VERSION*. Merged with version-independent
   package-requirements for *OPERATING-SYSTEM-TYPE*.

   Example values: for OPERATING-SYSTEM-VERSION when
   OPERATING-SYSTEM-TYPE is \"ubuntu\": \"precise\", \"trusty\".

     \"OPERATING-SYSTEM-TYPE\" {
         \"OPERATING-SYSTEM-VERSION\": {
             \"ARCHITECTURE\": {
                 \"packages\": [ \"PACKAGE₁\", \"PACKAGE₂\", … ]
             }
         }
     }

   A list of packages that have to be installed when building on a
   ARCHITECTURE machine with an operating system of type
   OPERATING-SYSTEM-TYPE with version OPERATING-SYSTEM-VERSION.
   Merged with the previous two entries.

   Example values: \"i686\", \"x86_64\".")

;;; Job variables

(define-variable :build-job-name string
  "Name of the Jenkins job that should be generated.")

(define-variable :kind (or string (cons string (cons string null)))
  "The kind of the Jenkins job that should be generated.

   Typically a string

     \"KIND\"

   where KIND is \"project\" or \"matrix\".")

(define-variable :dependency-job-name string
  "Internal.")

(define-variable :dependencies.mode string ; TODO (member :direct :minimal :none) would be nice
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

(define-variable :upstream-dir string
  "Directory in which artifacts copied from upstream jobs should be
   placed.")

;; Buildflow variables

(define-variable :disable-orchestration-jobs boolean
  "If true, do not generate a buildflow job and the associated prepare
   and finish jobs for a distribution.")

(define-variable :buildflow-name string
  "Name of the buildflow job that should be generated for a
   distribution.")

(define-variable :buildflow.parallel? boolean
  "If true, generate a buildflow in which jobs that are know to be
   paralellizable are built in parallel to some extent.")

(define-variable :buildflow.exclude? boolean
  "If true in a project, do include any Jenkins jobs associated to the
   project in generated buildflow jobs.")

(define-variable :prepare-hook-name string
  "The name of a Jenkins job that should run before a distribution is
   built.")

(define-variable :prepare-hook/unix string
  "A fragment of shell code that should be run by the \"prepare-hook\"
   job.")

(define-variable :finish-hook-name string
  "The name of a Jenkins job that should run after a distribution has
   been built.")

(define-variable :finish-hook/unix string
  "A fragment of shell code that should be run by the \"finish-hook\"
   job.")

;;; View variables

(define-variable :view.create? boolean
  "If true, create an associated view for each created distribution.")

(define-variable :view.name string
  "The name of the associated view for the current distribution.

   Has no effect if the value of view.create? is not true.")
