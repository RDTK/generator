;;;; aspects.lisp --- Expressing aspects as Dockerfile operations.
;;;;
;;;; Copyright (C) 2018-2022 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:build-generator.deployment.dockerfile)

(defmethod aspects:extend! ((aspect t)
                            (spec   t)
                            (output dockerfile-job)
                            (target (eql :dockerfile)))
  output)

(defmethod aspects:extend! ((aspect aspects::aspect-builder-defining-mixin)
                            (spec   t)
                            (output dockerfile-job)
                            (target (eql :dockerfile)))
  (when-let* ((command (aspects:extend! aspect spec 'string :command))
              (step    (shell-command aspect command))
              (name    (class-name (class-of aspect))) ; TODO (aspects:tag aspect) would be better
              (tag     (or (find-symbol (subseq (symbol-name name) (length "ASPECT-"))
                                        '#:build-generator.model.aspects)
                           (error "Something is wrong with the aspect tag of ~A" aspect))))
    (aspects::register-constraints aspect 'aspects::build step tag '())
    (push step (builders output)))
  output)

;;; TODO duplicated in model/aspects/protocol.lisp
(defmethod aspects:extend! ((aspect list)
                            (spec   t)
                            (output dockerfile-job)
                            (target (eql :dockerfile)))
  ;; Apply aspects, respecting declared ordering, and sort generated
  ;; steps (i.e. builders and publishers) according to declared
  ;; ordering.
  (let ((aspects::*step-constraints* '())
        (aspects (util:sort-with-partial-order
                  (copy-list aspect) #'aspects:aspect<)))

    ;; Methods on `extend!' add entries to `*step-constraints*' and
    ;; push builders onto (builders output).
    (reduce (lambda (output aspect)
              (aspects:extend! aspect spec output target))
            aspects :initial-value output)

    (let ((constraints (aspects::constraints-table 'aspects::build))
          (unsorted    (builders output)))
      (when unsorted
        (log:trace "~@<~@(~A~)er constraint~P:~@:_~
                      ~@<~{• ~{~
                        ~A ~A:~A ~@:_~
                        ~2@T~@<~/build-generator.model.aspects::format-constraints/~@:>~
                      ~}~^~@:_~}~@:>~
                    ~@:>"
                   'aspects::build
                   (hash-table-count constraints)
                   (hash-table-alist constraints))

        ;; Try to sort steps according to CONSTRAINTS.
        (let ((sorted (util:sort-with-partial-order
                       unsorted (rcurry #'aspects::step< constraints))))
          (log:debug "~@<Sorted builder~P for ~A:~@:_~
                      ~@<~{• ~A~^~@:_~}~@:>~@:>"
                      (length sorted) output sorted)
          (setf (builders output) sorted)))))

  output)

;;; Individual aspects

(defmethod aspects:extend! ((aspect aspects::aspect-archive)
                            (spec   t)
                            (output dockerfile-job)
                            (target (eql :dockerfile)))
  (when-let* ((command (aspects:extend! aspect spec 'string :command))
              (step    (shell-command aspect command)))
    (aspects::register-constraints aspect 'aspects::build step 'aspects::archive '((:before t)))
    (push step (builders output)))
  output)

(defmethod aspects:extend! ((aspect aspects::aspect-sloccount)
                            (spec   t)
                            (output dockerfile-job)
                            (target (eql :dockerfile)))
  output)

(defmethod aspects:extend! ((aspect aspects::aspect-git)
                            (spec   t)
                            (output dockerfile-job)
                            (target (eql :dockerfile)))
  (when-let* ((command (with-output-to-string (stream)
                         (aspects:extend! aspect spec stream :command)
                         (aspects:extend! aspect spec stream :sub-directory-command)))
              (step    (unless (emptyp command)
                         (shell-command aspect command))))
    (aspects::register-constraints aspect 'aspects::build step 'aspects::git '())
    (push step (builders output)))
  output)

(defmethod aspects:extend! ((aspect aspects::aspect-mercurial)
                            (spec   t)
                            (output dockerfile-job)
                            (target (eql :dockerfile)))
  (when-let* ((command (with-output-to-string (stream)
                         (aspects:extend! aspect spec stream :command)
                         (aspects:extend! aspect spec stream :sub-directory-command)))
              (step    (unless (emptyp command)
                         (shell-command aspect command))))
    (aspects::register-constraints aspect 'aspects::build step 'aspects::mercurial '())
    (push step (builders output)))
  output)
