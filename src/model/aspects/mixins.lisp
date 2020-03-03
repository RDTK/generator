;;;; mixins.lisp --- Mixin classes for aspect classes.
;;;;
;;;; Copyright (C) 2012-2020 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:build-generator.model.aspects)

(define-constant +all-marker+ "<all>"
  :test #'string=)

(defun+ parse-constraint ((&whole raw kind subject))
  (let+ (((&flet parse-kind ()
            (make-keyword (string-upcase kind))))
         ((&flet parse-class-or-tag (value)
            (if (or (equal value +all-marker+) (eq value t))
                t
                (intern (string-upcase value) #.*package*))))
         ((&flet parse-name (value)
            (if (or (equal value +all-marker+) (eq value t))
                t
                value))))
    (cond
      ((equal subject +all-marker+)
       (list (parse-kind) t t))
      ((stringp subject)
       (list (parse-kind) (parse-class-or-tag subject) t))
      ((consp subject)
       (let+ (((&plist-r/o (class-or-tag :type t) (name :name t))
               (alist-plist subject)))
         (list (parse-kind)
               (parse-class-or-tag class-or-tag)
               (parse-name name))))
      (t
       (error 'type-error
              :datum         raw
              :expected-type '(or (eql +all-marker+) cons))))))

(defun constraints-table (phase)
  (let ((index *step-constraints*))
    (or (assoc-value index phase)
        (let* ((table (make-hash-table :test #'eq))
               (index (acons phase table index)))
          (setf *step-constraints* index)
          table))))

(defun register-constraints (aspect phase step tag constraints
                             &key (target-phase phase)
                                  step-string)
  (let* ((constraints (append constraints
                              (step-constraints aspect phase (or step-string step))))
         (cell        (ensure-gethash
                       step (constraints-table target-phase)
                       (list tag (model:name aspect) '()))))
    (log:trace "~@<All constraints for ~A~@:_~
                ~/build-generator.model.aspects::format-constraints/~@:>"
               step constraints)
    (appendf (third cell) constraints)))

;;; `aspect-builder-defining-mixin'

(defclass aspect-builder-defining-mixin ()
  ()
  (:documentation
   "Adds processing of builder ordering constraints."))

(defmethod step-constraints ((aspect aspect-builder-defining-mixin)
                             (phase  (eql 'build))
                             (step   t))
  (let+ ((step-string     (typecase step
                            (string
                             step)
                            (t
                             (let ((type-string (string (type-of step))))
                               (subseq type-string (length "builder/"))))))
         (variable        (format-symbol
                           :keyword "ASPECT.BUILDER-CONSTRAINTS.~@:(~A~)"
                           step-string))
         (constraints/raw (var:value aspect variable nil))
         (constraints     (mapcar #'parse-constraint constraints/raw)))
    (log:trace "~@<Constraints for ~A in ~A~:@_~
                ~/build-generator.model.aspects::format-constraints/~@:>"
               step variable constraints)
    constraints))

;;; `aspect-publisher-defining-mixin'

(defclass aspect-publisher-defining-mixin ()
  ()
  (:documentation
   "Adds processing of publisher ordering constraints."))

(defmethod step-constraints ((aspect aspect-publisher-defining-mixin)
                             (phase  (eql 'publish))
                             (step   t))
  (let+ ((step-string     (typecase step
                            (string
                             step)
                            (t
                             (let ((type-string (string (type-of step))))
                               (subseq type-string (length "builder/"))))))
         (variable        (format-symbol
                           :keyword "ASPECT.PUBLISHER-CONSTRAINTS.~@:(~A~)"
                           step-string))
         (constraints/raw (var:value aspect variable nil))
         (constraints     (mapcar #'parse-constraint constraints/raw)))
    (log:trace "~@<Constraints for ~A in ~A~:@_~
                ~/build-generator.model.aspects::format-constraints/~@:>"
               step variable constraints)
    constraints))
