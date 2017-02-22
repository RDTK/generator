(cl:in-package #:jenkins.model.project)

(defmethod deploy ((thing project-automation.model.project.stage4:distribution))
  (mapc #'deploy (rosetta-project.model.project:versions thing)))

(defmethod deploy ((thing project-automation.model.project.stage4:distribution-version))
  (mapc #'deploy (project-automation.model.project:direct-includes thing))
  (mapc #'deploy (project-automation.model.project:direct-projects thing)))

(defmethod deploy ((thing project-automation.model.project.stage4:project-version))
                                        ; TODO (mapc #'deploy jobs)

  )

(trace deploy)
