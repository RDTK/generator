;;;; protocol.lisp --- Protocol provided by the model.aspects module.
;;;;
;;;; Copyright (C) 2012-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.model.aspects)

;;; Aspect protocol

(defgeneric aspect< (left right)
  (:documentation
   "Return non-nil if the aspect LEFT should be applied before the
    aspect RIGHT."))

(defgeneric builder-constraints (aspect builder)
  (:documentation
   "Return a list of ordering constraints of one of the forms

      (:before (t | TAG) )
      (:after  (t | TAG) )

    for BUILDER created and configured by ASPECT."))

(defgeneric builder< (left right constraints)
  (:documentation
   "Return non-nil if CONSTRAINTS mandate that the builder LEFT should
    be executed before the builder RIGHT."))

(defgeneric extend! (job aspect spec)
  (:method-combination progn))

;; Default behavior

(defvar *builder-constraints* nil)

(defmethod extend! progn ((job t) (aspect list) (spec t))
  ;; Apply aspects, respecting declared ordering, and sort generated
  ;; builders according to declared ordering.
  (let ((*builder-constraints* (make-hash-table))
        (aspects (sort-with-partial-order (copy-list aspect) #'aspect<)))
    ;; Methods on `extend!' add entries to `*builder-constraints*'
    ;; and push builders onto (builders job).
    (reduce (lambda (job aspect)
              (restart-case
                  (extend! job aspect spec)
                (continue (&optional condition)
                  :report (lambda (stream)
                            (format stream "~@<Do not apply ~A to ~A.~@:>"
                                    aspect job))
                  (declare (ignore condition))
                  job)))
            aspects :initial-value job)

    (log:trace "Builder constraints: ~S"
               (hash-table-alist *builder-constraints*))

    ;; Try to sort builders according to `*builder-constraints*'.
    (setf (builders job)
          (sort-with-partial-order
           (builders job) (rcurry #'builder< *builder-constraints*)))

    (log:trace "Sorted builders: ~A" (builders job))))

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
