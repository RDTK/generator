;;;; aspect.lisp --- Basic infrastructure for job aspects.
;;;;
;;;; Copyright (C) 2012-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.model.aspects)

(defclass aspect (named-mixin
                  implementation-mixin
                  direct-variables-mixin
                  parented-mixin)
  ((constraints :initarg  :constraints
                :type     list
                :reader   constraints
                :initform '()
                :documentation
                "Constraints regarding the ordering of aspect
                 execution imposed by this aspect. Entries are of one
                 of the forms:

                   (:after  ASPECT-NAME)
                   (:before ASPECT-NAME)

                 where ASPECT-NAME designates the aspect to which this
                 aspect is related by the ordering constraint."))
  (:documentation
   "Instances of this class correspond to particular aspect of a
    generated build job such as checking out source code from a
    repository or parsing compiler messages and publishing a
    report."))

(defmethod check-access ((object aspect) (lower-bound t))
  (check-access (specification (parent (parent object))) lower-bound))

(defmethod aspect< ((left aspect) (right aspect))
  (let+ (((&accessors-r/o (constraints-left constraints))  left)
         ((&accessors-r/o (constraints-right constraints)) right)
         ((&flet+ satisfied? ((kind &rest args) other transpose?)
            (ecase kind
              (:after  (and transpose?       (typep other (first args))))
              (:before (and (not transpose?) (typep other (first args))))))))
    (or (some (rcurry #'satisfied? right nil) constraints-left)
        (some (rcurry #'satisfied? left t)    constraints-right))))

(defmethod builder-constraints ((aspect t) (builder t))
  '())

(defmethod builder< ((left t) (right t) (constraints hash-table))
  (let+ (((&optional tag-left  name-left  constraints-left)  (gethash left constraints))
         ((&optional tag-right name-right constraints-right) (gethash right constraints))
         ((&flet tag-matches? (query tag &optional (allow-wild? t))
            (or (and allow-wild? (eq query t)) (eq query tag))))
         ((&flet name-matches? (query name &optional (allow-wild? t))
            (or (and allow-wild? (eq query t))
                (and (stringp query) (string= query name)))))
         ((&flet+ matches? ((query-tag &optional (query-name t)) (tag name)
                            allow-wild-tag? allow-wild-name?)
            (and (tag-matches?  query-tag  tag  allow-wild-tag?)
                 (name-matches? query-name name allow-wild-name?))))
         ((&flet+ satisfied? ((kind &rest spec) other
                              transpose? allow-wild-tag? allow-wild-name?)
            (when-let ((result (matches? spec other allow-wild-tag? allow-wild-name?)))
              (ecase kind
                (:after  transpose?)
                (:before (not transpose?))))))
         ((&flet some-satiesfied? (allow-wild-tag? allow-wild-name? direction)
            (or (some (rcurry #'satisfied? (list tag-right name-right)
                              (eq direction '>) allow-wild-tag? allow-wild-name?)
                      constraints-left)
                (some (rcurry #'satisfied? (list tag-left  name-left)
                              (eq direction '<) allow-wild-tag? allow-wild-name?)
                      constraints-right))))
         ((&flet test-consistent-satisfied (allow-wild-tag? allow-wild-name?)
            (cond
              ((and (some-satiesfied? allow-wild-tag? allow-wild-name? '<)
                    (not (some-satiesfied? allow-wild-tag? allow-wild-name? '>)))
               (return-from builder< t))
              ((and (not (some-satiesfied? allow-wild-tag? allow-wild-name? '<))
                    (some-satiesfied? allow-wild-tag? allow-wild-name? '>))
               (return-from builder< nil))))))
    (test-consistent-satisfied nil nil)
    (test-consistent-satisfied t   nil)
    (test-consistent-satisfied nil t)
    (test-consistent-satisfied t   t)))
