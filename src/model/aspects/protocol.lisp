;;;; protocol.lisp --- Protocol provided by the model.aspects module.
;;;;
;;;; Copyright (C) 2012-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.model.aspects)

;;; Aspect parameter protocol

(defgeneric aspect-parameter-variable (parameter)
  (:documentation
   "Return the `variable-info' instance associated to PARAMETER."))

(defgeneric aspect-parameter-binding-name (parameter)
  (:documentation
   "Return the name of the variable that should be bound to the value
    of PARAMETER."))

(defgeneric aspect-parameter-default-value (parameter)
  (:documentation
   "Return the default value of parameter.

    Return two values: 1) nil or the default value of PARAMETER 2) a
    Boolean indicating whether PARAMETER has a default value."))

;;; Aspect protocol

(defgeneric aspect-parameters (aspect)
  (:method ((aspect class))
    (unless (closer-mop:class-finalized-p aspect)
      (closer-mop:finalize-inheritance aspect))
    (aspect-parameters (closer-mop:class-prototype aspect)))
  (:documentation
   "Return a list of parameters accepted by ASPECT.

    Elements of the returned list implement the aspect parameter
    protocol."))

(defgeneric aspect-process-parameters (aspect)
  (:documentation
   "Obtain values for ASPECT's parameters from its variables, return a
    list of values."))

(defgeneric aspect-process-parameter (aspect parameter)
  (:documentation
   "Lookup PARAMETER in ASPECTS's variables, return value in a singleton list.

    The returned list is of then form

      (VALUE)

    where VALUE is looked up via the variable name specified by
    PARAMETER's `aspect-parameter-variabe'.

    When the variable is not defined and PARAMETER does not define a
    default value, a `missing-argument-error' is signaled.

    When the variable is defined but its value is not of the type
    specified by PARAMETER's `aspect-parameter-type', an
    `argument-type-error' is signaled."))

(defgeneric aspect< (left right)
  (:documentation
   "Return non-nil if the aspect LEFT should be applied before the
    aspect RIGHT."))

(defgeneric step-constraints (aspect phase step)
  (:documentation
   "Return a list of ordering constraints of one of the forms

      (:before (t | TAG) )
      (:after  (t | TAG) )

    for STEP which is created and configured by ASPECT in phase
    PHASE. PHASE currently is either `build' or `publish'."))

(defgeneric step< (left right constraints)
  (:documentation
   "Return non-nil if CONSTRAINTS mandate that the step LEFT should be
    executed before the step RIGHT."))

(defgeneric extend! (job aspect spec)
  (:method-combination progn))

;; Default behavior

(declaim (type (or null (cons (cons symbol hash-table) list))
               *step-constraints*))
(defvar *step-constraints* nil)

(defmethod step-constraints ((aspect t) (phase t) (step t))
  '())

(defmethod extend! progn ((job t) (aspect list) (spec t))
  ;; Apply aspects, respecting declared ordering, and sort generated
  ;; steps (i.e. builders and publishers) according to declared
  ;; ordering.
  (let+ ((*step-constraints* '())
         (aspects (sort-with-partial-order (copy-list aspect) #'aspect<))
         ((&flet sort-phase (phase read write)
            (let ((unsorted    (funcall read job))
                  (constraints (constraints-table phase)))
              (when unsorted
                (log:trace "~@<~@(~A~)er constraint~P:~@:_~
                              ~@<~{• ~{~
                                ~A ~A:~A ~@:_~
                                ~2@T~@<~/jenkins.model.aspects:format-constraints/~@:>~
                              ~}~^~@:_~}~@:>~
                            ~@:>"
                           phase (hash-table-count constraints)
                           (hash-table-alist constraints))

                ;; Try to sort steps according to CONSTRAINTS.
                (let ((sorted (sort-with-partial-order
                               unsorted (rcurry #'step< constraints))))
                  (log:debug "~@<Sorted ~(~A~)er~P:~@:_~
                              ~@<~{• ~A~^~@:_~}~@:>~@:>"
                             phase (length sorted) sorted)
                  (funcall write sorted job)))))))

    ;; Methods on `extend!' add entries to `*step-constraints*' and
    ;; push builders onto (builders job).
    (reduce (rcurry #'extend! spec) aspects :initial-value job)

    (sort-phase 'build   #'builders   #'(setf builders))
    (sort-phase 'publish #'publishers #'(setf publishers))))

;;; Aspect creation protocol

(defgeneric make-aspect (spec &rest initargs)
  (:documentation
   "Make and return the provider specified by SPEC.

    INITARGS are passed to the constructed provider."))

(defmethod make-aspect ((spec symbol) &rest initargs)
  (apply #'service-provider:make-provider 'aspect
         spec initargs))

(defmethod make-aspect ((spec string) &rest initargs)
  (let ((name (make-keyword (string-upcase spec))))
    (apply #'make-aspect name initargs)))

(service-provider:define-service aspect
  (:documentation
   "Providers of this service are aspect classes.

    Instances of an aspect class accept certain variables as their
    parameters and configure one aspect (hence the name) of a
    generated build job according to the value of those parameters."))

;;; Aspect container protocol

(defgeneric aspects (container)
  (:documentation
   "Return the list of aspects contained in CONTAINER."))
