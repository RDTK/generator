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

;;; `project-rules'

(defclass project-rules (model:implementation-mixin
                         aspects::aspect-builder-defining-mixin)
  ((%directory :initarg  :directory
               :reader   directory)
   (%rules     :initarg  :rules
               :type     list
               :accessor rules
               :initform '()))
  (:default-initargs
   :directory (more-conditions:missing-required-initarg 'project-rules :directory))
  (:documentation
   "A collection of `rule' instances for one project."))

(defun make-project-rules (specification directory)
  (make-instance 'project-rules :directory     directory
                                :specification specification))

;;; `rule'

(defclass rule (deploy:command-mixin
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
   :name (more-conditions:missing-required-initarg 'rule :name)))

(defun make-rule (name command &key (dependencies '()) early? builder-class)
  (make-instance 'rule :name          name
                       :command       command
                       :dependencies  dependencies
                       :early?        early?
                       :builder-class builder-class))

(defmethod print-items:print-items append ((object rule))
  `(((:name (:before :command)) "~A " ,(name object))))

(defmethod aspects::step-constraints ((aspect aspects::aspect-builder-defining-mixin)
                                      (phase  (eql 'aspects::build))
                                      (step   rule))
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

(defun make-ensure-directory-rule (directory)
  (make-rule "ensure-directory" (format nil "mkdir -p '~A'" directory)))

;;;

(defun write-rule (stream name &key dependencies directory command comment)
  (let* ((rule-name (util:safe-name name))
         (log-file  (format nil "~A.log" rule-name))
         (prefix    (string #\Tab)))
    ;; Write comment and rule head.
    (format stream "~@[# ~A~%~]~
                    ~A:~{ ~A~}~@
                    "
            comment rule-name (map 'list #'util:safe-name dependencies))
    ;; Write rule body (called "recipe" in the make documentation).
    (when command
      (let ((shell-string (escape-dollars (maybe-base64-encode command))))
        (pprint-logical-block (stream (list command) :per-line-prefix prefix)
          (format stream "@~
                          echo -en '\\e[1mExecuting ~A\\e[0m\\n'~@
                          +(~@
                            set -e~@
                            ~@[~
                              cd '~A'~@
                              export WORKSPACE=\"$$(pwd)\"~@
                            ~]~
                            ~@
                            ~A~@:_~
                          ) > '~A' 2>&1~@
                          if [ $$? -ne 0 ] ; then~@
                          ~2@Techo -en '\\e[35m'~@
                          ~2@Tcat '~:*~A'~@
                          ~2@Techo -en '\\e[0m'~@
                          ~2@Texit 1~@
                          fi~@
                          touch '~4:*~A'"
                  rule-name directory shell-string log-file)))
      (terpri stream))
    (terpri stream)))

(defmethod deploy:deploy ((thing project::job) (target makefile-target))
  (let* ((directory (deploy:job-full-name thing))
         (output    (make-project-rules thing directory)))
    (push output (model:implementations thing))

    ;; Apply aspects, respecting declared ordering, and sort generated
    ;; builders according to declared ordering.
    (aspects:extend! (aspects:aspects thing) thing output :makefile)

    output))

(defun write-project-rules (stream thing)
  (let+ ((specification        (model:specification thing))
         (name                 (deploy:job-full-name specification))
         (directory            (directory thing))
         (rules                (rules thing))
         (project-dependencies (map 'list #'deploy:job-full-name
                                    (model:direct-dependencies
                                     specification)))
         (ensure-directory     (make-ensure-directory-rule
                                directory))
         ((&flet rule-name (rule)
            (format nil "~A-~A" name (name rule)))))
    ;; Header/separator
    (deploy:print-heading stream name)

    ;; Preparation rule
    (write-rule stream (rule-name ensure-directory)
                :command (deploy:command ensure-directory))

    ;; Actual rules
    (map nil (lambda (rule)
               (with-simple-restart
                   (continue "~@<Skip ~A~@:>" rule)
                 (let ((dependencies (append
                                      (map 'list #'rule-name
                                           (list* ensure-directory
                                                  (dependencies rule)))
                                      (unless (early? rule)
                                        project-dependencies))))
                   (write-rule stream (rule-name rule)
                               :dependencies dependencies
                               :directory    directory
                               :command      (deploy:command rule)))))
         rules)

    ;; Interface rule
    (write-rule stream name :dependencies (map 'list #'rule-name rules))
    name))

(defmethod deploy:deploy ((thing sequence) (target makefile-target))
  (unless (every (of-type 'project:distribution) thing)
    (return-from deploy:deploy (call-next-method)))

  (let ((deployed-things (call-next-method))
        (makefile        (merge-pathnames "Makefile" (output-directory target)))
        (project-rules   '())
        (interface-rules '()))
    ;; Generate rule text for all projects and collect the names of
    ;; interface rules.
    (map nil (lambda (thing)
               (with-simple-restart (continue "~@<Skip ~A~@:>" thing)
                 (let* ((stream         (make-string-output-stream))
                        (interface-rule (write-project-rules stream thing)))
                   (push (get-output-stream-string stream) project-rules)
                   (push interface-rule interface-rules))))
         deployed-things)

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
                      ~2%")

      ;; Add an "all" rule for convenience.
      (write-rule stream "all" :dependencies interface-rules)

      ;; Write project rules.
      (format stream "~{~A~^~2%~}" project-rules))))
