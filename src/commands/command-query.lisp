;;;; command-query.lisp --- Query recipe information.
;;;;
;;;; Copyright (C) 2017, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.project.commands)

(defclass query ()
  ((recipe-directory :initarg :recipe-directory
                     :type    configuration.options:directory-pathname
                     :reader  recipe-directory
                     :documentation
                     "Directory from which recipes should be collected.")
   (query            :initarg :query
                     :type    string
                     :reader  query
                     :documentation
                     "TODO"))
  (:default-initargs
   :recipe-directory (missing-required-initarg 'query :recipe-directory)
    :query            (missing-required-initarg 'query :query))
  (:documentation
   "Query recipes."))

(service-provider:register-provider/class
 'command :query :class 'query)

(jenkins.project.commandline-options:define-option-mapping
    (*command-schema* "query")
  (0 "recipe-directory" "DIRECTORY" t)
  (1 "query"            "QUERY"     t))

(defmethod command-execute ((command query))
  (let+ ((pattern            (merge-pathnames
                              "distributions/*.distribution"
                              (uiop:ensure-directory-pathname
                               (recipe-directory command))))
         (distribution-files (directory pattern))
         ((&values distributions projects)
          (generate-load
           distribution-files "toolkit" '()
           :generator-version (generator-version))))
    ;; execute query
    ))
