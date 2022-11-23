;;;; aspect.lisp --- Basic infrastructure for job aspects.
;;;;
;;;; Copyright (C) 2012-2019, 2022 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:build-generator.model.aspects)

;;; `aspect-parameter' class

(defclass aspect-parameter (print-items:print-items-mixin)
  ((variable      :initarg  :variable
                  :type     var:variable-info
                  :reader   aspect-parameter-variable
                  :documentation
                  "Stores a `variable-info' instance for the variable
                   potentially containing the value of this
                   parameter.")
   (default-value :initarg  :default-value
                  :reader   aspect-parameter-%default-value
                  :documentation
                  "Default value to use in case the associated
                   variable does not define a value for the
                   parameter.

                   Unbound when there is no default value.")
   (binding-name  :initarg  :binding-name
                  :type     (and symbol (not null))
                  :reader   aspect-parameter-binding-name
                  :documentation
                  "Stores a name to which the value of the parameter,
                   if supplied, should be bound in the aspect body."))
  (:default-initargs
   :variable     (missing-required-initarg 'aspect-parameter :variable)
   :binding-name (missing-required-initarg 'aspect-parameter :binding-name))
  (:documentation
   "Instances of this class describe parameters accepted by aspects."))

(defmethod print-items:print-items append ((object aspect-parameter))
  (let+ (((&structure-r/o
           aspect-parameter- variable binding-name
           ((&values default default?) default-value))
          object)
         ((&accessors-r/o (type var:variable-info-type)) variable))
    `((:binding-name                         "~A"  ,binding-name)
      ((:type        (:after :binding-name)) ":~A" ,type)
      ,@(when default?
          `(((:default (:after :type)) " = ~S" ,default))))))

(defmethod aspect-parameter-default-value ((parameter aspect-parameter))
  (if (slot-boundp parameter 'default-value)
      (values (slot-value parameter 'default-value) t)
      (values nil                                   nil)))

;;; `aspect' base class

(defclass aspect (model:named-mixin
                  model:implementation-mixin
                  model:parented-mixin
                  var:direct-variables-mixin)
  ((required-plugins :allocation :class
                     :accessor required-plugins
                     :initform '()
                     :documentation
                     "Stores a list of names of plugin required by the
                      aspect.")
   (constraints      :initarg  :constraints
                     :type     list
                     :reader   constraints
                     :initform '()
                     :documentation
                     "Constraints regarding the ordering of aspect
                      execution imposed by this aspect. Entries are of
                      one of the forms:

                        (:after  ASPECT-NAME)
                        (:before ASPECT-NAME)

                      where ASPECT-NAME designates the aspect to which
                      this aspect is related by the ordering
                      constraint."))
  (:documentation
   "Instances of this class correspond to particular aspect of a
    generated build job such as checking out source code from a
    repository or parsing compiler messages and publishing a
    report."))

(defmethod aspect-process-parameter ((aspect t) (parameter t))
  (let+ (((&structure-r/o aspect-parameter- variable
                          ((&values default default?) default-value))
          parameter)
         ((&accessors-r/o (name var:variable-info-name)
                          (type var:variable-info-type))
          variable)
         (value (var:value aspect name '%undefined)))
    (cond
      ;; HACK treat nil permissively
      ((and (null value) (not (typep value type)))
       (if (and default? (equal value default))
           (list value)
           (throw '%bail nil)))

      ((not (eq value '%undefined))
       (with-condition-translation
           (((error argument-type-error)
             :aspect    aspect
             :parameter parameter
             :value     value))
         (list (var:as value type))))
      ((not default?)
       (missing-argument-error aspect parameter))
      ((equal default '%bail)
       (throw '%bail nil))
      (t
       (list default)))))

(defmethod aspect-process-parameters ((aspect t))
  (mappend (curry #'aspect-process-parameter aspect)
           (aspect-parameters aspect)))

(defmethod model:check-access ((object aspect) (lower-bound t))
  (model:check-access (model:parent (model:parent object)) lower-bound))

(defmethod aspect< ((left aspect) (right aspect))
  (let+ (((&accessors-r/o (constraints-left constraints))  left)
         ((&accessors-r/o (constraints-right constraints)) right)
         ((&flet+ satisfied? ((kind &rest args) other transpose?)
            (ecase kind
              (:after  (and transpose?       (typep other (first args))))
              (:before (and (not transpose?) (typep other (first args))))))))
    (or (some (rcurry #'satisfied? right nil) constraints-left)
        (some (rcurry #'satisfied? left t)    constraints-right))))

(defmethod step< ((left t) (right t) (constraints hash-table))
  (let+ (((&optional tag-left  name-left  constraints-left)
          (gethash left constraints))
         ((&optional tag-right name-right constraints-right)
          (gethash right constraints))
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
               (return-from step< t))
              ((and (not (some-satiesfied? allow-wild-tag? allow-wild-name? '<))
                    (some-satiesfied? allow-wild-tag? allow-wild-name? '>))
               (return-from step< nil))))))
    (test-consistent-satisfied nil nil)
    (test-consistent-satisfied t   nil)
    (test-consistent-satisfied nil t)
    (test-consistent-satisfied t   t)))
