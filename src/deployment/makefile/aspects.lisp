;;;; aspects.lisp --- Aspect extensions used in the deployment.makefile module.
;;;;
;;;; Copyright (C) 2018-2022 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:build-generator.deployment.makefile)

(defmethod aspects:extend! ((aspect t)
                            (spec   t)
                            (output project-rule-infos)
                            (target (eql :makefile)))
  output)

(defmethod aspects:extend! ((aspect aspects::aspect-builder-defining-mixin)
                            (spec   t)
                            (output project-rule-infos)
                            (target (eql :makefile)))
  (when-let* ((command (aspects:extend! aspect spec 'string :command))
              (name    (class-name (class-of aspect))) ; TODO (aspects:tag aspect) would be better
              (tag     (or (find-symbol (subseq (symbol-name name) (length "ASPECT-"))
                                        '#:build-generator.model.aspects)
                           (error "Something is wrong with the aspect tag of ~A" aspect)))
              (info    (make-instance 'rule-info
                                      :name          (model:name aspect)
                                      :command       command
                                      :builder-class tag)))
    (aspects::register-constraints aspect 'aspects::build info tag '())
    (push info (rule-infos output)))
  output)

(defmethod aspects:extend! ((aspect list)
                            (spec   t)
                            (output project-rule-infos)
                            (target (eql :makefile)))
  ;; Apply aspects, respecting declared ordering, and sort generated
  ;; steps (i.e. builders and publishers) according to declared
  ;; ordering.
  (let+ ((aspects::*step-constraints* '())
         (aspects (util:sort-with-partial-order
                   (copy-list aspect) #'aspects:aspect<)))

    ;; Methods on `extend!' add entries to `*step-constraints*' and
    ;; push builders onto (rule-infos output).
    (reduce (lambda (output aspect)
              (aspects:extend! aspect spec output target))
            aspects :initial-value output)

    (let ((constraints (aspects::constraints-table 'aspects::build))
          (rule-infos  (rule-infos output)))
      (when rule-infos
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
        (mapc (lambda (info)
                (setf (dependencies info)
                      (remove-if-not (rcurry #'aspects::step< info constraints)
                                     rule-infos)))
             rule-infos))))

  output)

;;; Individual aspect classes

(defmethod aspects:extend! ((aspect aspects::aspect-archive)
                            (spec   t)
                            (output project-rule-infos)
                            (target (eql :makefile)))
  (let* ((command (aspects:extend! aspect spec 'string :command))
         (info    (make-instance 'rule-info :name    (model:name aspect)
                                            :command command
                                            :early?  t)))
    (aspects::register-constraints
     aspect 'aspects::build info 'aspects::archive '((:before t)))
    (push info (rule-infos output)))
  output)

(defmethod aspects:extend! ((aspect aspects::aspect-sloccount)
                            (spec   t)
                            (output project-rule-infos)
                            (target (eql :makefile)))
  output)

(defmethod aspects:extend! ((aspect aspects::aspect-git)
                            (spec   t)
                            (output project-rule-infos)
                            (target (eql :makefile)))
  (when-let* ((command (with-output-to-string (stream)
                         (aspects:extend! aspect spec stream :command)
                         (aspects:extend! aspect spec stream :sub-directory-command)))
              (info    (unless (emptyp command)
                         (make-instance 'rule-info
                                        :name          (model:name aspect)
                                        :command       command
                                        :early?        t
                                        :builder-class 'aspects::git))))
    (aspects::register-constraints aspect 'aspects::build info 'aspects::git '())
    (push info (rule-infos output)))
  output)

(defmethod aspects:extend! ((aspect aspects::aspect-mercurial)
                            (spec   t)
                            (output project-rule-infos)
                            (target (eql :makefile)))
  (when-let* ((command (with-output-to-string (stream)
                         (aspects:extend! aspect spec stream :command)
                         (aspects:extend! aspect spec stream :sub-directory-command)))
              (info    (unless (emptyp command)
                         (make-instance 'rule-info
                                        :name          (model:name aspect)
                                        :command       command
                                        :early?        t
                                        :builder-class 'aspects::mercurial))))
    (aspects::register-constraints
     aspect 'aspects::build info 'aspects::mercurial '())
    (push info (rule-infos output)))
  output)
