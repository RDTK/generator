((:release "0.22")

 (:release "0.21"

  (:incompatible-change
   "The commandline interface expects a sub-command that determines
    what should be done (instead of always generating Jenkins
    jobs). The previous behavior is mostly provided by the
    \"generate\" command with some differences:"
   (:ul
    "The distribution(s) are now specified as positional arguments,
     i.e. without \"-d\"."
    "Platform requirements are neither checked nor reported. The new
     \"platform-requirements\" command does that in a more sensible
     way."
    "It is no longer possible to generate jobs and reports in the same
     invocation. The new \"report\" command is responsible for
     generating reports. ")
   "Currently supported commands are:"
   (:ul
    (:verb "analyze                Analyze project repositories w.r.t. dependencies and meta-data.")
    (:verb "generate               Generate Jenkins jobs for a given distribution.")
    (:verb "help                   Print help either for all commands or for a given command.")
    (:verb "info-aspects           Print information about available aspects.")
    (:verb "info-variables         Print information about recognized variables.")
    (:verb "platform-requirements  Analyze system packages required on a given platform.")
    (:verb "report                 Generate one or more reports for given distribution(s).")
    (:verb "version                Print the version of this program and some components.")))

  (:incompatible-change
   "The configuration schema change, in particular affecting
    the" (:file "build-generator.conf") "configuration file."
   (:ul
    ("The old" (:code "general") "section is now
      called" (:cold "global") ":"
     (:ul
      (:verb "general.cache-directory -> global.cache-directory")
      (:verb "general.temp-directory  -> global.temp-directory")
      (:verb "general.num-processes   -> global.num-processes")
      (:verb "general.progress-style  -> global.progress-style")))
    ("The old" (:code "jenkis") "section no longer exists. Most of its
      options are now specific to the \"generate\" command:"
     (:ul
      (:verb "jenkins.base-uri             -> commands.generate.base-uri")
      (:verb "jenkins.username             -> commands.generate.username")
      (:verb "jenkins.password             -> commands.generate.password")
      (:verb "jenkins.api-token            -> commands.generate.api-token")
      (:verb "jenkins.delete-other         -> commands.generate.delete-other?")
      (:verb "jenkins.delete-other-pattern -> commands.generate.delete-other-pattern"))))
   "Setting the environment
    variable" (:code "BUILD_GENERATOR_CONFIG_DEBUG") "can help
    debugging configuration problems.")

  (:enhancement
   "Source location is reported for missing project files included in
    distributions.")

  (:bugfix
   "The" (:variable "recipe.maintainer") "variable is no longer
    inherited from distributions in projects.")

  (:enhancement
   "The" (:variable "build-job.disabled?") "variable now accepts the
    value" (:code "force-disabled") ".")

  (:enhancement
   "Upgrading cache entries written by an older generator version is
    no longer treated as an error.")

  (:enhancement
   "Analysis of projects with" (:code "ros_package ") "nature provides
    Maven artifacts in the" (:code "org.ros.rosjava_messages") "group.")

  (:enhancement
   "Improved committer analysis")

  (:enhancement
   "Improved mercurial support")

  (:enhancement
   "The new variable" (:variable "view.colums") "controls columns in
    created views."))

 (:release "0.20"

  (:enhancement
   "The" (:code "name") "property is now optional in all recipe
    kinds (It has already been optional in templates).")

  (:enhancement
   "In distributions, variable references are expanded in included
    project versions. However, the expression can only refer variables
    defined directly in the distribution.")

  (:enhancement
   "References to non-existent project versions are reported with
    source location.")

  (:enhancement
   "When running interactively in a capable terminal, the \"one-line\"
    progress style is used by default.")

  (:bugfix
   "The" (:code "mps") "analysis no longer gets confused by symbolic
   links."))

 (:release "0.19"

  (:incompatible-change
   "Generated Jenkins jobs for projects with
    archive" (:variable "scm") "now
    respect" (:variable "sub-directory") "properly. Recipes that
    worked around this bug have to be adjusted.")

  (:enhancement
   "Many recipe-related errors are now reported alongside an excerpt
    from the file highlighting relevant source locations.")

  (:enhancement
   "When generating Jenkins jobs, the ability to communicate with the
    specified Jenkins server is ensured first, failing early in case
    there is a problem."))

 (:release "0.18"

  (:enhancement
   "Versions defined in the" (:code "versions") "section of project
    recipes can now specialize more variables. In particular, the
    following variables now work as expected in version blocks:"
   (:ul
    "repository"
    "scm"
    "scm.username"
    "scm.password"
    "sub-directory"
    "branch"
    "tag"
    "commit"
    "directory"
    "natures"))

  (:enhancement
   "Jenkins' cross-site request forgery (CSRF) protection is now
    supported.")

  (:bugfix
   "Updating permissions in generated Jenkins jobs now works
    properly.")

  (:enhancement
   "The analysis of project licenses is now more robust and more
    efficient.")

  (:bugfix
   "The analysis of projects with" (:code "cmake") "nature handles
    sub-directories more robustly.")

  (:enhancement
   "A certain kind of Jetbrains MPS projects can now be analyzed using
    the" (:code "mps") "nature.")))
