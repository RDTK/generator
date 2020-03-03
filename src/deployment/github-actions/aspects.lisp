;;;; aspects.lisp --- Aspect extensions used in the deployment.github-actions module.
;;;;
;;;; Copyright (C) 2020, 2022 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:build-generator.deployment.github-actions)

(defmethod aspects:extend! ((aspect t)
                            (spec   t)
                            (output job)
                            (target (eql :github-actions)))
  output)

(defmethod aspects:extend! ((aspect list)
                            (spec   t)
                            (output job)
                            (target (eql :github-actions)))
  ;; Apply aspects, respecting declared ordering, and sort generated
  ;; steps (i.e. builders and publishers) according to declared
  ;; ordering.
  (let+ ((aspects::*step-constraints* '())
         (aspects (util:sort-with-partial-order
                   (copy-list aspect) #'aspects:aspect<)))

    ;; Methods on `extend!' add entries to `*step-constraints*' and
    ;; push builders onto (builders job).
    (reduce (lambda (output aspect)
              (aspects:extend! aspect spec output target))
            aspects :initial-value output)

    (let ((constraints (aspects::constraints-table 'aspects::build))
          (unsorted    (steps output)))
      (when unsorted
        (log:trace "~@<~@(~A~)er constraint~P:~@:_~
                      ~@<~{• ~{~
                        ~A ~A:~A ~@:_~
                        ~2@T~@<~/build-generator.model.aspects:format-constraints/~@:>~
                      ~}~^~@:_~}~@:>~
                    ~@:>"
                   'aspects::build (hash-table-count constraints)
                   (hash-table-alist constraints))

        ;; Try to sort steps according to CONSTRAINTS.
        (let ((sorted (util:sort-with-partial-order
                       unsorted (rcurry #'aspects::step< constraints))))
          (log:trace "~@<Sorted builder~P:~@:_~
                      ~@<~{• ~A~^~@:_~}~@:>~@:>"
                     (length sorted) sorted)
          (setf (steps output) sorted)))))

  output)

;;; Properties

(defmethod aspects:extend! ((aspect aspects::aspect-timeout)
                            (spec   t)
                            (output job)
                            (target (eql :github-actions)))
  (catch 'aspects::%bail
    (apply
     (lambda (timeout/minutes)
       (setf (timeout output) timeout/minutes))
     (aspects::aspect-process-parameters aspect)))
  output)

;;; SCM

(defmethod aspects:extend! ((aspect aspects::aspect-archive)
                            (spec   t)
                            (output job)
                            (target (eql :github-actions)))
  (let* ((command (aspects:extend! aspect spec 'string :command))
         (step    (command-step (model:name aspect) command)))
    (aspects::register-constraints aspect 'aspects::build step 'aspects::archive '((:before t)))
    (push step (steps output)))
  output)

(defmethod aspects:extend! ((aspect aspects::aspect-git)
                            (spec   t)
                            (output job)
                            (target (eql :github-actions)))
  (catch 'aspects::%bail
    (apply
     (lambda (url username password credentials branches local-branch
              clone-timeout wipe-out-workspace? clean-before-checkout?
              checkout-submodules? shallow? sub-directory)
       (let* ((name (model:name aspect))
              (step (if nil
                        (make-github-checkout name url (first branches)) ; TODO
                        (let ((command (with-output-to-string (stream)
                                         (aspects:extend! aspect spec stream :command)
                                         (when sub-directory
                                           (aspects:extend! aspect spec stream :sub-directory-command)))))
                          (command-step name command)))))
         (aspects::register-constraints aspect 'aspects::build step 'aspects::git '()
                                        :step-string "GIT")
         (push step (steps output))))
     (aspects::aspect-process-parameters aspect)))
  output)

(defmethod aspects:extend! ((aspect aspects::aspect-subversion)
                            (spec   t)
                            (output job)
                            (target (eql :github-actions)))
  (catch 'aspects::%bail
    (apply
     (lambda (url revision credentials local-dir checkout-strategy)
       (let* ((command (aspects:extend! aspect spec 'string :command))
              (step    (command-step (model:name aspect) command
                                     :builder-class 'aspects::subversion)))
         (aspects::register-constraints aspect 'aspects::build step 'aspects::subversion '()
                                        :step-string "SUBVERSION")
         (push step (steps output))))
     (aspects::aspect-process-parameters aspect)))
  output)

;;; Builders

(defmethod aspects:extend! ((aspect aspects::aspect-shell)
                            (spec   t)
                            (output job)
                            (target (eql :github-actions)))
  (catch 'aspects::%bail
    (apply
     (lambda (command)
       (let ((step (command-step (model:name aspect) command
                                        ; :builder-class 'aspects::shell
                                 )))
         (aspects::register-constraints aspect 'aspects::build step 'aspects::shell '())
         (push step (steps output))))
     (aspects::aspect-process-parameters aspect)))
  output)

(defmethod aspects:extend! ((aspect aspects::aspect-cmake/unix)
                            (spec   t)
                            (output job)
                            (target (eql :github-actions)))
  (catch 'aspects::%bail
    (let ((step (command-step (model:name aspect)
                              (aspects:extend! aspect spec 'string :command)
                              :builder-class 'aspects::cmake/unix)))
      (aspects::register-constraints aspect 'aspects::build step 'aspects::cmake/unix '()
                                     :step-string "CMAKE/UNIX")
      (push step (steps output))))
  output)

;;;

(defmethod aspects:extend! ((aspect aspects::aspect-archive-artifacts)
                            (spec   t)
                            (output job)
                            (target (eql :github-actions)))
  (catch 'aspects::%bail
    (apply
     (lambda (file-pattern)
       (when file-pattern
         (let* ((patterns (ensure-list file-pattern))
                (gather   (command-step "gather-artifacts"
                                        (format nil "mkdir -p artifacts~@
                                                     mv -v ~{~A~^ ~} artifacts/"
                                                patterns))))
           (aspects::register-constraints
            aspect 'aspects::publish gather 'gather-artifacts '()
            :target-phase 'aspects::build :step-string "ARCHIVE-ARTIFACTS")
           (push gather (steps output)))
         (let* ((name   (deploy:job-full-name spec))
                (upload (make-upload-artifact name "artifacts")))
           (aspects::register-constraints
            aspect 'aspects::publish upload 'upload-artifacts '((:after gather-artifacts))
            :target-phase 'aspects::build :step-string "ARCHIVE-ARTIFACTS")
           (push upload (steps output)))))
     (aspects::aspect-process-parameters aspect)))
  output)

(defmethod aspects:extend! ((aspect aspects::aspect-dependency-download)
                            (spec   t)
                            (output job)
                            (target (eql :github-actions)))
  (let ((copy-artifacts? nil))
    (when-let ((upstream-dir (var:value spec :upstream-dir nil))
               (self-kind    (first (ensure-list (var:value spec :kind nil))))
               (dependencies (model:dependencies spec)))
      ;; Multiple copy-artifact builders which copy artifacts from
      ;; other jobs.
      (map nil (lambda (dependency)
                 (let+ ((kind    (first (ensure-list (var:value dependency :kind nil))))
                        (name    (deploy:job-full-name dependency))
                        (pattern (when-let ((aspect (find-if (of-type 'aspects::aspect-archive-artifacts)
                                                             (aspects:aspects dependency))))
                                   (var:value/cast aspect :aspect.archive-artifacts.file-pattern nil)))
                        ((&flet matrix? (kind)
                           (member kind '("matrix" "matrix-project") :test #'string-equal)))
                        #+no (reference (format nil "~A~@[/label=$label~]"
                                                id (matrix? kind))))
                   (cond ((not pattern)
                          (log:info "~@<Upstream project ~A does not provide ~
                                        archived artifacts to copy into downstream ~
                                        workspaces (variable ~S has no value).~@:>"
                                    dependency :aspect.archive-artifacts.file-pattern))
                         ((and (matrix? kind) (not (matrix? self-kind)))
                          (error "~@<Upstream job ~A is of kind ~A, downstream ~
                          job ~A is of kind ~A.~@:>"
                                 dependency kind output self-kind))
                         (t
                          (let ((step (make-download-artifact name upstream-dir)))
                            (aspects::register-constraints aspect 'aspects::build step 'aspects::copy-artifact '((:after sloccount)))
                            (push step (steps output)))
                          (setf copy-artifacts? t)))))
           dependencies)

      ;; Shell builder which unpacks dependencies. Has to run after
      ;; artifact download, obviously.
      (when copy-artifacts?
        (let* ((command (format nil "cd ~A~@
                                  find . -name '*.tar.gz' -exec tar -xzf '{}' \\;"
                                upstream-dir))
               (step    (command-step "extract-upstream-artiacts" command)))
          #+todo (aspects::constraint! (aspects::build ((:before t) (:after copy-artifact)))
                                       )
          (push step (steps output))))))
  output)
