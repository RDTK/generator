;;;; aspects.lisp --- Aspect extensions used in the deployment.makefile module.
;;;;
;;;; Copyright (C) 2018-2022 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:build-generator.deployment.makefile)

(defmethod aspects:extend! ((aspect t)
                            (spec   t)
                            (output project-rules)
                            (target (eql :makefile)))
  output)

(defmethod aspects:extend! ((aspect aspects::aspect-builder-defining-mixin)
                            (spec   t)
                            (output project-rules)
                            (target (eql :makefile)))
  (when-let* ((command (aspects:extend! aspect spec 'string :command))
              (name    (class-name (class-of aspect))) ; TODO (aspects:tag aspect) would be better
              (tag     (or (find-symbol (subseq (symbol-name name) (length "ASPECT-"))
                                        '#:build-generator.model.aspects)
                           (error "Something is wrong with the aspect tag of ~A" aspect)))
              (rule    (make-rule (model:name aspect) command :builder-class tag)))
    (aspects::register-constraints aspect 'aspects::build rule tag '())
    (push rule (rules output)))
  output)

(defmethod aspects:extend! ((aspect list)
                            (spec   t)
                            (output project-rules)
                            (target (eql :makefile)))
  ;; Apply aspects, respecting declared ordering, and sort generated
  ;; steps (i.e. builders and publishers) according to declared
  ;; ordering.
  (let+ ((aspects::*step-constraints* '())
         (aspects (util:sort-with-partial-order
                   (copy-list aspect) #'aspects:aspect<)))

    ;; Methods on `extend!' add entries to `*step-constraints*' and
    ;; push builders onto (rules output).
    (reduce (lambda (output aspect)
              (aspects:extend! aspect spec output target))
            aspects :initial-value output)

    (let ((constraints (aspects::constraints-table 'aspects::build))
          (rules       (rules output)))
      (when rules
        (log:debug "~@<~@(~A~)er constraint~P:~@:_~
                      ~@<~{• ~{~
                        ~A ~A:~A ~@:_~
                        ~2@T~@<~/build-generator.model.aspects:format-constraints/~@:>~
                      ~}~^~@:_~}~@:>~
                    ~@:>"
                   'aspects::build
                   (hash-table-count constraints)
                   (hash-table-alist constraints))
        ;; Compute rule dependencies based on aspect constraints.
        (mapc (lambda (rule)
                (setf (dependencies rule)
                      (remove-if-not (rcurry #'aspects::step< rule constraints)
                                     rules)))
             rules))))

  output)

;;; Individual aspect classes

(defmethod aspects:extend! ((aspect aspects::aspect-archive)
                            (spec   t)
                            (output project-rules)
                            (target (eql :makefile)))
  (let* ((command (aspects:extend! aspect spec 'string :command))
         (rule    (make-rule (model:name aspect) command :early? t)))
    (aspects::register-constraints aspect 'aspects::build rule 'aspects::archive '((:before t)))
    (push rule (rules output)))
  output)

(defmethod aspects:extend! ((aspect aspects::aspect-sloccount)
                            (spec   t)
                            (output project-rules)
                            (target (eql :makefile)))
  output)

(defmethod aspects:extend! ((aspect aspects::aspect-git)
                            (spec   t)
                            (output project-rules)
                            (target (eql :makefile)))
  (when-let* ((command (with-output-to-string (stream)
                         (aspects:extend! aspect spec stream :command)
                         (aspects:extend! aspect spec stream :sub-directory-command)))
              (rule    (unless (emptyp command)
                         (make-rule (model:name aspect) command
                                    :early? t
                                    :builder-class 'aspects::git))))
    (aspects::register-constraints aspect 'aspects::build rule 'aspects::git '())
    (push rule (rules output)))
  output)

(defmethod aspects:extend! ((aspect aspects::aspect-mercurial)
                            (spec   t)
                            (output project-rules)
                            (target (eql :makefile)))
  (when-let* ((command (with-output-to-string (stream)
                         (aspects:extend! aspect spec stream :command)
                         (aspects:extend! aspect spec stream :sub-directory-command)))
              (rule    (unless (emptyp command)
                         (make-rule (model:name aspect) command
                                    :early?        t
                                    :builder-class 'aspects::mercurial))))
    (aspects::register-constraints aspect 'aspects::build rule 'aspects::mercurial '())
    (push rule (rules output)))
  output)
