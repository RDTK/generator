((:release "0.23" nil

  (:enhancement
   "The YAML syntax for recipes now supports including other YAML
    documents as well as text files:"
   (:ul
    ("A tagged scalar node of the form"
     (:verb "!b!include FILENAME")
     "causes" (:verb "FILENAME") "to be loaded as a YAML file and
      spliced into the current recipe, replacing the tagged scalar
      node.")
    ("A tagged scalar node of the form"
     (:verb "!b!literal-include FILENAME")
     "causes" (:verb "FILENAME") "to be read as a string and spliced
      into the current recipe, replacing the tagged scalar node.")))

  (:enhancement
   "Better error reports for syntax errors and missing templates in
    YAML recipes.")

  (:enhancement
   "During the analysis phase, certain kinds of failed HTTP downloads
    now lead to a limited number of retries instead of an immediate
    failure.")

  (:enhancement
   "Errors during report generation are now reported with their own
    error kind.")

  (:enhancement
   "Information for Gitea repositories can now be generated into the
    configuration of the Jenkins Git plugin.")

  (:enhancement
   "The" (:verb "analyze") "command now handles URIs and filesystem
    paths properly.")

  (:enhancement
   "A warning is no longer emitted when automatically switching from
    JSON to YAML syntax for a recipe.")

  (:bugfix
   "A long-standing problem related non-ASCII characters in Jenkins
    job data has been fixed by working around the way Jenkins handles
    HTTP content types and XML encoding declarations."))

 (:release "0.22" "2018-06-08"

  (:enhancement
   "Recipes can now be written using YAML syntax following (mostly)
    the same structural schema as before. As JSON is a subset of YAML,
    the legacy JSON syntax and in particular existing recipes are
    still supported. The following differences are noteworthy:"
   (:ul
    ("Comments are now allowed and can be written as" (:verb "# comment"))
    ("Many string literals can be written in vastly improved manner:"
     (:ul
      "Quotes and escaping are not necessary in most cases"
      "Multi-line strings pose no problems"
      "A suitable interpretation of surrounding whitespace and
       indentation can be specified as appropriate"))
    ("The" (:verb "name") "attribute is not allowed in project,
      distribution and template recipes")
    ("The inclusion of projects in distributions can now be written in
      several different ways:"
     (:ul
      ("Old way for compatibility:"
       (:verb "[ \"NAME\", VERSION₁, VERSION₂, …]") "where
       each" (:verb "VERSION") "is either a string or"
       (:verb "[ \"NAME\", { \"PARAMETER₁\": \"VALUE₁\", … } ] "))
      ("New way, compact:" (:verb "NAME@VERSION") "or"
       (:verb "NAME … @VERSION") "to allow the"
       (:verb "NAME₁             @VERSION₁
MUCH-LONGER-NAME₂ @VERSION₂")
       "idiom")
      ("New way, single version with parameters:"
       (:verb "name: NAME
version: VERSION
parameters:
  PARAMETER₁: VALUE₁
  PARAMETER₂: VALUE₂"))
      ("New way, multiple versions with or without parameters:"
       (:verb "name: NAME
versions:
- version: VERSION₁
  parameters:
    PARAMETER₁₁: VALUE₁₁
    PARAMETER₁₂: VALUE₁₂
- version: VERSION₂"))))
    ("Dependency specifications can now be written in several
     different ways:"
     (:ul
      ("Old way for compatibility:"
       (:verb "[ \"NATURE\", \"TARGET\" ]") "or"
       (:verb "[ \"NATURE\",\"TARGET\", VERSION ]") "where"
       (:verb "VERSION") "is either a string or a list of version
        component strings")
      ("New way:"
       (:verb "nature: NATURE
target: TARGET
version: VERSION")
       "where the" (:verb "version") "property is optional")))))

  (:enhancement
   "The new" (:verb "validate") "command checks the syntax and
    referential integrity of recipes in a given recipe repository.")

  (:enhancement
   "Analysis results for projects using" (:variable "scm")
   (:verb "archive") "can now be cached.")

  (:enhancement
   "Better error messages for missing or wrong arguments to
    commandline options.")

  (:enhancement
   "More cases of ill-formed recipes are caught at parse-time.")

  (:bugfix
   "The" (:verb "trace-variable") "commandline option has been
    restored.")

  (:bugfix
   "The" (:verb "generate") "command accepts a"
   (:verb "-a API-TOKEN") "commandline argument like the old
    commandline interface.")

  (:bugfix
   "Supplying single-value commandline options multiple times now
    produces a suitable error message."))

 (:release "0.21" "2018-05-07"

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
   "The configuration schema changed, affecting environment variables
    and the" (:file "build-generator.conf") "configuration file:"
   (:ul
    ("The old" (:code "general") "section is now
      called" (:cold "global") ":"
     (:ul
      (:verb "general.cache-directory → global.cache-directory")
      (:verb "general.temp-directory  → global.temp-directory")
      (:verb "general.num-processes   → global.num-processes")
      (:verb "general.progress-style  → global.progress-style")))
    ("The old" (:code "jenkis") "section no longer exists. Most of its
      options are now specific to the \"generate\" command:"
     (:ul
      (:verb "jenkins.base-uri             → commands.generate.base-uri")
      (:verb "jenkins.username             → commands.generate.username")
      (:verb "jenkins.password             → commands.generate.password")
      (:verb "jenkins.api-token            → commands.generate.api-token")
      (:verb "jenkins.delete-other         → commands.generate.delete-other?")
      (:verb "jenkins.delete-other-pattern → commands.generate.delete-other-pattern"))))
   "Setting the environment
    variable" (:code "BUILD_GENERATOR_CONFIG_DEBUG") "can help
    debugging configuration problems.")

  (:enhancement
   "Source locations are reported for missing project files included
    in distribution recipes.")

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
   "The analysis of repository committers has been improved.")

  (:enhancement
   "Support for the mercurial version control system has been
    improved.")

  (:enhancement
   "The new variable" (:variable "view.colums") "controls columns in
    created views."))

 (:release "0.20" "2018-03-02"

  (:enhancement
   "The" (:code "name") "property is now optional in all recipe
    kinds (It has already been optional in template recipes).")

  (:enhancement
   "In distributions, variable references are expanded in included
    project versions. However, the expression can only refer variables
    defined directly in the distribution.")

  (:enhancement
   "References to non-existent project versions are reported with
    source locations.")

  (:enhancement
   "When running interactively in a capable terminal, the \"one-line\"
    progress style is used by default.")

  (:bugfix
   "The" (:code "mps") "analysis no longer gets confused by symbolic
   links."))

 (:release "0.19" "2018-02-15"

  (:incompatible-change
   "Generated Jenkins jobs for projects with
    archive" (:variable "scm") "now respect the value of
    the" (:variable "sub-directory") " variable properly. Recipes that
    worked around this bug may have to be adjusted.")

  (:enhancement
   "Many recipe-related errors are now reported alongside an excerpt
    from the file highlighting relevant source locations.")

  (:enhancement
   "When generating Jenkins jobs, the ability to communicate with the
    specified Jenkins server is ensured before performing long-running
    operations such as project analyses, failing early in case there
    is a problem."))

 (:release "0.18" "2018-01-02"

  (:enhancement
   "Versions defined in the" (:code "versions") "section of project
    recipes can now overwrite more variables. In particular, the
    following variables now work as expected in version blocks:"
   (:ul
    (:variable "repository")
    (:variable "scm")
    (:variable "scm.username")
    (:variable "scm.password")
    (:variable "sub-directory")
    (:variable "branch")
    (:variable "tag")
    (:variable "commit")
    (:variable "directory")
    (:variable "natures")))

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
