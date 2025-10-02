;;;; target.lisp --- Target definition for generating a Makefile.
;;;;
;;;; Copyright (C) 2018-2023 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:build-generator.deployment.makefile)

;;; `makefile-target'

(defclass makefile-target ()
  ((output-directory :initarg :output-directory
                     :type    (and pathname (satisfies uiop:directory-pathname-p))
                     :reader  output-directory
                     :documentation
                     #.(format nil "The directory into which the ~
                        Makefile and the associated scripts should ~
                        be written.")))
  (:documentation
   "Write a Makefile that builds or install the specified projects."))

(service-provider:register-provider/class
 'deploy:target :makefile :class 'makefile-target)

;;; `project-rule-infos'

(defclass project-rule-infos (model:implementation-mixin
                              aspects::aspect-builder-defining-mixin)
  ((%directory  :initarg  :directory
                :reader   directory)
   (%rule-infos :initarg  :rules
                :type     list
                :accessor rule-infos
                :initform '()))
  (:default-initargs
   :directory (more-conditions:missing-required-initarg 'project-rule-infos :directory))
  (:documentation
   "A collection of `rule-info' instances for one project."))

(defun make-project-rule-infos (specification directory)
  (make-instance 'project-rules :directory     directory
                                :specification specification))

;;; `rule-info'

(defclass rule-info (deploy:command-mixin
                     print-items:print-items-mixin)
  ((%name         :initarg  :name
                  :type     string
                  :reader   name)
   (%dependencies :initarg  :dependencies
                  :type     list
                  :accessor dependencies
                  :initform '())
   (%early?       :initarg  :early?
                  :type     boolean
                  :reader   early?
                  :initform nil
                  :documentation
                  "Controls whether the rule can be executed
                   \"early\", that is disregarding inter-project
                   dependencies.")
   ;; HACK
   (%builder-class :initarg :builder-class
                   :reader builder-class
                   :initform nil))
  (:default-initargs
   :name (more-conditions:missing-required-initarg 'rule-info :name)))

(defun make-rule-info (name command &key (dependencies '()) early? builder-class)
  (make-instance 'rule-info :name          name
                            :command       command
                            :dependencies  dependencies
                            :early?        early?
                            :builder-class builder-class))

(defmethod print-items:print-items append ((object rule))
  `(((:name (:before :command)) "~A " ,(name object))))

(defmethod aspects::step-constraints ((aspect aspects::aspect-builder-defining-mixin)
                                      (phase  (eql 'aspects::build))
                                      (step   rule-info))
  (when-let ((builder-class (builder-class step)))
    (let* ((variable        (let ((*package* (find-package '#:keyword)))
                              (symbolicate  '#:aspect.builder-constraints.
                                            builder-class)))
           (constraints/raw (var:value aspect variable nil))
           (constraints     (mapcar #'aspects::parse-constraint constraints/raw)))
      (log:debug "~@<Constraints for ~A in ~A~:@_~
                  ~/build-generator.model.aspects::format-constraints/~@:>"
                 step variable constraints)
      constraints)))

;;;

(defmethod deploy:deploy ((thing project::job) (target makefile-target))
  (let* ((directory (deploy:job-full-name thing))
         (output    (make-instance 'project-rule-infos
                                   :specification thing
                                   :directory     directory)))
    (push output (model:implementations thing))

    ;; Apply aspects, respecting declared ordering, and sort generated
    ;; builders according to declared ordering.
    (aspects:extend! (aspects:aspects thing) thing output :makefile)

    output))

(defun make-ensure-directory-rule (directory)
  (make-instance 'rule-info :name    "ensure-directory"
                            :command (format nil "mkdir -p '~A'" directory)))

(defun finalize-project-rules (thing &key extra-dependencies)
  (let+ ((specification        (model:specification thing))
         (name                 (deploy:job-full-name specification))
         (directory            (directory thing))
         (rule-infos           (rule-infos thing))
         (project-dependencies (map 'list #'deploy:job-full-name
                                    (model:direct-dependencies
                                     specification)))
         (ensure-directory     (make-ensure-directory-rule directory))
         (final-rules          '()))
    (labels ((full-name (rule-name)
               (format nil "~A-~A" name rule-name))
             (rule-name (rule-info)
               (full-name (name rule-info)))
             (add-rule (name command &rest args)
               (push (apply #'make-instance 'rule :name    name
                                                  :command command
                                                  args)
                     final-rules)))
      ;; Header/separator
      ;; (deploy:print-heading stream name)
      ;; Interface rule
      (add-rule name nil :dependencies (map 'list #'rule-name rule-infos))
      ;; Preparation rule
      (add-rule (rule-name ensure-directory) (deploy:command ensure-directory)
                :dependencies extra-dependencies)
      ;; Actual rules
      (map nil (lambda (rule-info)
                 (with-simple-restart (continue "~@<Skip ~A~@:>" rule-info)
                   (let ((dependencies (append
                                        (map 'list #'rule-name
                                             (list* ensure-directory
                                                    (dependencies rule-info)))
                                        (unless (early? rule-info)
                                          project-dependencies)
                                        extra-dependencies)))
                     (add-rule (rule-name rule-info) (deploy:command rule-info)
                               :dependencies dependencies
                               :directory    directory))))
           rule-infos)
      (values (nreverse final-rules) name))))

(defmethod deploy:deploy ((thing sequence) (target makefile-target))
  (unless (every (of-type 'project:distribution) thing)
    (return-from deploy:deploy (call-next-method)))

  (let* ((deployed-things         (call-next-method))
         (directory               (output-directory target))
         (makefile                (merge-pathnames "Makefile" directory))
         (rules                   '())
         (interface-rule-names    '())
         (prepare-hook-rule-names '())
         (finish-hook-rule-names  '()))
    ;; Prepare hooks
    (map nil (lambda (distribution)
               (when-let ((command (var:value distribution :prepare-hook/unix nil)))
                 (let* ((name      (model:name distribution))
                        (rule-name (format nil "~A-~(~A~)" name :prepare-hook/unix))
                        (rule      (make-instance 'rule :name      rule-name
                                                        :command   command
                                                        :directory directory)))
                   (push rule-name prepare-hook-rule-names)
                   (appendf rules (list rule)))))
         thing)
    ;; Generate rules for all projects and collect the names of
    ;; interface rules.
    (map nil (lambda (thing)
               (with-simple-restart (continue "~@<Skip ~A~@:>" thing)
                 (multiple-value-bind (project-rules interface-rule-name)
                     (finalize-project-rules
                      thing :extra-dependencies prepare-hook-rule-names)
                   (appendf rules project-rules)
                   (push interface-rule-name interface-rule-names))))
         deployed-things)
    ;; Finish hooks
    (map nil (lambda (distribution)
               (when-let ((command (var:value distribution #2=:finish-hook/unix nil)))
                 (let* ((name         (model:name distribution))
                        (rule-name    (format nil "~A-~(~A~)" name #2#))
                        (dependencies (append prepare-hook-rule-names
                                              interface-rule-names))
                        (rule         (make-instance 'rule :name         rule-name
                                                           :dependencies dependencies
                                                           :command      command
                                                           :directory    directory)))
                   (push rule-name finish-hook-rule-names)
                   (appendf rules (list rule)))))
         thing)
    ;; Write the Makefile.
    (ensure-directories-exist makefile)
    (with-output-to-file (stream makefile :if-exists :supersede)
      (deploy:print-heading stream "This file is automatically generated.")

      ;; Execute the recipe lines of each rule as a single shell chunk
      ;; of shell instead of one shell invocation per line (or
      ;; multiple continuation lines using "\"). Note that we have to
      ;; pass -c to the shell so it doesn't attempt to execute the
      ;; shell code chunk as a command.
      (format stream ".ONESHELL:~@
                      SHELL = /bin/bash~@
                      .SHELLFLAGS = -c~@
                      ~%")
      (format stream ".PHONY: ~{~A~^ ~}~3%"
              (list* "all" (map 'list #'name (remove-if-not #'phony? rules))))

      ;; Add an "all" rule for convenience.
      (write-rule stream "all" :dependencies (append interface-rule-names
                                                     finish-hook-rule-names))

      ;; Write project rules.
      (map nil (lambda (rule)
                 (write-rule stream (name rule)
                             :dependencies (dependencies rule)
                             :directory    (directory rule)
                             :command      (deploy:command rule)
                                        ; :comment      (comment rule)
                             )
                 (format stream "~2%"))
           rules))))
