;;;; aspects.lisp --- Aspect extensions used in the deployment.build module.
;;;;
;;;; Copyright (C) 2018-2022 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:build-generator.deployment.build)

(defun step-name (aspect)
  (format nil "~{~A~^.~}" (reverse (model:ancestor-names aspect))))

(defmethod aspects::step-constraints ((aspect aspects::aspect-builder-defining-mixin)
                                      (phase  (eql 'aspects::build))
                                      (step   step))
  (when-let ((builder-class (builder-class step)))
    (let* ((variable        (let ((*package* (find-package '#:keyword)))
                              (symbolicate  '#:aspect.builder-constraints.
                                            builder-class)))
           (constraints/raw (var:value aspect variable nil))
           (constraints     (mapcar #'aspects::parse-constraint constraints/raw)))
      (log:debug "~@<Constraints for ~A in ~A~:@_~
                  ~/aspects::format-constraints/~@:>"
                 step variable constraints)
      constraints)))

(defmethod aspects:extend! ((aspect list)
                            (spec   t)
                            (output project-steps)
                            (target (eql :build)))
  ;; Apply aspects, respecting declared ordering. Translate builder
  ;; ordering constraints into step dependencies.
  (let+ ((aspects::*step-constraints* '())
         (aspects (util:sort-with-partial-order
                   (copy-list aspect) #'aspects:aspect<)))

    ;; Methods on `extend!' add entries to `*step-constraints*' and
    ;; call (add-step STEP OUTPUT).
    (reduce (lambda (output aspect)
              (aspects:extend! aspect spec output target))
            aspects :initial-value output)

    (let ((constraints (aspects::constraints-table 'aspects::build))
          (steps       (steps output)))
      (when steps
        (log:debug "~@<~@(~A~)er constraint~P:~@:_~
                      ~@<~{• ~{~
                        ~A ~A:~A ~@:_~
                        ~2@T~@<~/build-generator.model.aspects:format-constraints/~@:>~
                      ~}~^~@:_~}~@:>~
                    ~@:>"
                   'aspects::build (hash-table-count constraints)
                   (hash-table-alist constraints))

        (map nil (lambda (step)
                   (setf (dependencies step)
                         (remove-if-not (rcurry #'aspects::step< step constraints)
                                        steps)))
             steps))))

  output)

(defmethod aspects:extend! ((aspect t)
                            (spec   t)
                            (output project-steps)
                            (target (eql :build)))
  output)

(defmethod aspects:extend! ((aspect aspects::aspect-builder-defining-mixin)
                            (spec   t)
                            (output project-steps)
                            (target (eql :build)))
  (when-let* ((command (aspects:extend! aspect spec 'string :command))
              (name    (class-name (class-of aspect))) ; TODO (aspects:tag aspect) would be better
              (tag     (or (find-symbol (subseq (symbol-name name) (length "ASPECT-"))
                                        '#:build-generator.model.aspects)
                           (error "Something is wrong with the aspect tag of ~A" aspect)))
              (step    (make-step (step-name aspect) command :builder-class tag)))
    (aspects::register-constraints aspect 'aspects::build step tag '())
    (add-step step output))
  output)

;;; Individual aspect classes

(defmethod aspects:extend! ((aspect aspects::aspect-archive)
                            (spec   t)
                            (output project-steps)
                            (target (eql :build)))
  (let* ((command (aspects:extend! aspect spec 'string :command))
         (step    (make-step (step-name aspect) command :early? t)))
    (aspects::register-constraints aspect 'aspects::build step 'aspects::archive '((:before t)))
    (add-step step output))
  output)

(defmethod aspects:extend! ((aspect aspects::aspect-sloccount)
                            (spec   t)
                            (output project-steps)
                            (target (eql :build)))
  output)

(defmethod aspects:extend! ((aspect aspects::aspect-git)
                            (spec   t)
                            (output project-steps)
                            (target (eql :build)))
  (when-let* ((command (with-output-to-string (stream)
                         (aspects:extend! aspect spec stream :command)
                         (aspects:extend! aspect spec stream :sub-directory-command)))
              (step    (unless (emptyp command)
                         (make-step (step-name aspect) command :early? t
                                                               :builder-class 'aspects::git))))
    (aspects::register-constraints aspect 'aspects::build step 'aspects::git '())
    (add-step step output))
  output)

(defmethod aspects:extend! ((aspect aspects::aspect-mercurial)
                            (spec   t)
                            (output project-steps)
                            (target (eql :build)))
  (when-let* ((command (with-output-to-string (stream)
                         (aspects:extend! aspect spec stream :command)
                         (aspects:extend! aspect spec stream :sub-directory-command)))
              (step    (unless (emptyp command)
                         (make-step (step-name aspect) command :early? t
                                                               :builder-class 'aspects::mercurial))))
    (aspects::register-constraints aspect 'aspects::build step 'aspects::mercurial '())
    (add-step step output))
  output)
