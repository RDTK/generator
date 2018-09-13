;;;; command-validate.lisp --- Validate a recipe repository.
;;;;
;;;; Copyright (C) 2017, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.project.commands)

(define-constant +validation-levels+
    #(:syntax :check-variables :analyze :instantiate :check-access)
  :test #'equalp)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun validation-level-name->index (level)
    (or (position level +validation-levels+)
        (error "~@<Unknown validation level: ~S.~@:>" level))))

(define-compiler-macro validation-level-name->index (&whole form level)
  (declare (notinline validation-level-name->index))
  (if (constantp level)
      (validation-level-name->index (eval level))
      form))

(deftype validation-level ()
  `(member ,@(coerce +validation-levels+ 'list)))

(defun validation-level>= (left right)
  (>= (validation-level-name->index left)
      (validation-level-name->index right)))

(define-compiler-macro validation-level>= (left right)
  `(>= (validation-level-name->index ,left)
       (validation-level-name->index ,right)))

(defparameter *known-natures*
  '("meta" "freestyle"
    "asdf" "maven" "cmake" "pkg-config" "setuptools" "autotools"
    "ros-package"
    "program" "library" "c-include"))

(defclass validate ()
  ((recipes          :initarg  :recipes
                     :type     pathname
                     :reader   recipes
                     :documentation
                     "Distribution recipe or root directory of recipe repository.")
   (validation-level :initarg  :validation-level
                     :type     validation-level
                     :reader   validation-level
                     :initform :syntax
                     :documentation
                     #.(format nil "Extent of the validation to be performed.~@
                        ~@
                        Each level includes the validation actions of ~
                        the previous level.~@
                        ~@
                        syntax~@
                        instantiate~@
                        check-variables~@
                        analyze~@
                        check-access.")))
  (:default-initargs
   :recipes (missing-required-initarg 'validate :recipes))
  (:documentation
   "Perform basic sanity checks for a given recipe repository."))

(service-provider:register-provider/class
 'command :validate :class 'validate)

(jenkins.project.commandline-options:define-option-mapping
    (*command-schema* "validate")
  (0 "recipes"          "FILENAME-OR-DIRECTORY" t)
  (1 "validation-level" "LEVEL"))

(defmethod command-execute ((command validate))
  (let+ ((level              (validation-level command))
         (recipes            (recipes command))
         (distribution-files
          (cond
            ((wild-pathname-p recipes)
             (directory recipes))
            ((equal (pathname-type recipes) "distribution")
             (list recipes))
            (t
             (directory
              (merge-pathnames
               "distributions/*.distribution"
               (uiop:ensure-directory-pathname recipes))))))
         ((&values distributions projects)
          (generate-load distribution-files "toolkit" '()
                         :generator-version (generator-version)))
         (projects
          (when (validation-level>= level :check-variables)
            (check-variables/early projects)))
         ((&values distributions analyzed-projects)
          (when (validation-level>= level :analyze)
            (generate-analyze distributions projects
                              :generator-version (generator-version)
                              :cache-directory   *cache-directory*
                              :temp-directory    *temp-directory*)))
         #+no (projects
          (when (validation-level>= level :instantiate)
            (as-phase (:instantiate/project)
              (instantiate-projects analyzed-projects distributions)))))
    (when (validation-level>= level :analyze)
      (check-dependencies analyzed-projects))
    (when (validation-level>= level :check-access)
      (check-distribution-access distributions))))

(defun check-project-variables (project variables
                                &key
                                (known-natures *known-natures*)
                                description-checker
                                title-checker)
  (map nil (lambda+ ((&structure-r/o variable-info- name type))
             (with-simple-restart
                 (continue "~@<Skip the variable ~A.~@:>" name)
               (handler-bind
                   ((undefined-variable-error
                     #'continue)
                    ((and error (not jenkins.model.project::annotation-condition))
                     (lambda (condition)
                       (error "~@<Error in variable ~A in ~A: ~A~@:>"
                              name project condition))))
                 (let+ (((&values value default?)
                         (jenkins.model.variables:value project name nil))
                        (value (unless default? (as value type))))
                   (when (not default?)
                     (case name
                       ((:extra-requires :extra-provides)
                        (loop :for dependency :in value
                              :for (nature) = (jenkins.model.project::parse-dependency-spec ; TODO parsing breaks source locations
                                               dependency)
                              :unless (member nature known-natures :test #'string-equal)
                              :do (jenkins.model.project::object-error
                                   (list (list nature "used here" :error))
                                   "~@<Suspicious nature ~S in ~S value.~@:>"
                                   nature name)))
                       (:description
                        (when description-checker
                          (unless (equal value "-none-") ; TODO hack
                            (funcall description-checker project value))))
                       (:__catalog
                        (when title-checker
                          (when-let ((title (assoc-value value :title)))
                            (funcall title-checker project title))))))))))
       variables))

#+alternative (defun check-project-variables (project)
  (loop :for (name) :in (variables project)
     :for variable = (find-variable name :if-does-not-exist nil)
     :when variable
     :do (with-simple-restart (continue "~@<Skip the variable ~A.~@:>" name)
           (handler-case
               (let ((value (as (jenkins.model.variables:value project name)
                                (variable-info-type variable))))
                 (when (member name '(:extra-requires :extra-provides))
                   (loop :for (nature) :in value
                      :unless (member nature '("meta" "freestyle"
                                               "asdf" "maven" "cmake" "pkg-config" "setuptools" "autotools"
                                               "program" "library" "c-include")
                                      :test #'string=)
                      :do (error "~@<Suspicious ~S value ~S.~@:>"
                                 name value))))
             (undefined-variable-error ())
             (error (condition)
               (error "~@<Error in variable ~A in ~A: ~A~@:>"
                      name project condition))))))

(defun make-uniqueness-checker (variable &key (test #'equal))
  (let ((values (make-hash-table :test test))
        (lock   (bt:make-lock "uniqueness checker")))
    (lambda (&optional project (value nil value-supplied?))
      (bt:with-lock-held (lock)
        (if value-supplied?
            (push (cons project value) (gethash value values '()))
            (maphash (lambda (value projects+values)
                       (with-simple-restart
                           (continue "~@<Ignore the repeated value.~@:>")
                         (unless (length= 1 projects+values)
                           (jenkins.model.project::object-error
                            (loop :for i                 :from 0
                                  :for (project . value) :in   projects+values
                                  :collect (list value ; TODO will not work for computed values
                                                 (format nil "~:R occurrence" (1+ i))
                                                 (if (zerop i) :info :error)))
                            "~@<The value \"~A\" of the ~S variable ~
                             occurs in multiple projects.~@:>" value
                             variable)))) values))))))

(jenkins.model.variables:define-variable :__catalog list)

(defun check-variables/early (projects)
  (as-phase (:check-variables)
    (let ((variables           (remove-if (lambda (variable)
                                            (member (variable-info-name variable)
                                                    '(:jobs.list
                                                      :jobs.dependencies
                                                      :jobs.dependencies/groovy)))
                                          (jenkins.model.variables:all-variables)))
          (description-checker (make-uniqueness-checker :description))
          (title-checker       (make-uniqueness-checker '(:catalog :title))))
      (with-sequence-progress (:check-variables projects)
        (lparallel:pmapc
         (lambda (project)
           (progress "~A" (project-spec-and-versions-spec project))
           (with-simple-restart
               (continue "~@<Skip project ~A.~@:>" project)
             (check-project-variables
              (project-spec-and-versions-spec project)
              variables
              :description-checker description-checker
              :title-checker       title-checker)))
         :parts 100 projects))
      ;; Report
      (funcall description-checker)
      (funcall title-checker))
    projects))

;;;

(defun check-project-dependencies (project)
  (mapc (lambda (project-version)
          (let ((requires/analysis (jenkins.model.project::%requires project-version))
                (requires/extra    (map 'list #'jenkins.model.project::parse-dependency-spec
                                        (value/cast project-version :extra-requires '()))))
            (when-let ((redundant (intersection requires/analysis requires/extra
                                                :test #'equalp :key #'second)))
              (error "~@<Redundant requirement specification: ~S~@:_~
                      ~2@TAnalysis: ~<~S~:>~@:_~
                      ~2@TExtra:    ~<~S~:>~@:>"
                     redundant (list requires/analysis) (list requires/extra))))
          (let ((provides/analysis (jenkins.model.project::%provides project-version))
                (provides/extra    (remove :meta (map 'list #'jenkins.model.project::parse-dependency-spec
                                                      (value/cast project-version :extra-provides '()))
                                           :key #'first)))
            (when-let ((redundant (intersection provides/analysis provides/extra
                                                :test #'equalp :key #'second)))
              (error "~@<Redundant provided specification: ~S~@:_~
                      ~2@TAnalysis: ~<~S~:>~@:_~
                      ~2@TExtra:    ~<~S~:>~@:>"
                     redundant (list provides/analysis) (list provides/extra)))))
        (versions project)))

(defun check-dependencies (projects)
  (as-phase (:check-dependencies)
    (with-sequence-progress (:check-dependencies projects)
      (mapc ;lparallel:pmapc
       (lambda (project)
         (progress "~A" project)
         (with-simple-restart
             (continue "~@<Skip project ~A.~@:>" project)
           (check-project-dependencies project)))
       ;; :parts 100
       projects))
    projects))
