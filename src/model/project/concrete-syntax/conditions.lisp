;;;; conditions.lisp --- Conditions used in the recipe concrete syntax.
;;;;
;;;; Copyright (C) 2016-2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.model.project)

;;; Source location conditions

(define-condition annotation-condition ()
  ((annotations :initarg  :annotations
                :accessor annotations
                :initform '())))

(defmethod print-object :around ((object annotation-condition) stream)
  (let ((annotations (annotations object)))
    (let ((*print-circle* nil))
      (pprint-logical-block (stream annotations)
        (call-next-method)
        (loop :repeat 2 :do (pprint-newline :mandatory stream))
        (text.source-location.print::print-annotations stream annotations)))))

(define-condition simple-object-error (error
                                       annotation-condition
                                       simple-condition)
  ())

(defun object-error (annotated-objects
                     &optional format-control &rest format-arguments)
  (if-let ((annotations
            (loop :for (object text kind) :in annotated-objects
                  :for location = (location-of object)
                  :when location
                  :collect (apply #'text.source-location:make-annotation
                                  location text (when kind (list :kind kind))))))
    (error 'simple-object-error
           :annotations      annotations
           :format-control   format-control
           :format-arguments format-arguments)
    (apply #'error format-control format-arguments)))

;;; YAML syntax

(define-condition yaml-syntax-error (error
                                     annotation-condition
                                     more-conditions:chainable-condition)
  ()
  (:report
   (lambda (condition stream)
     (let* ((cause (more-conditions:cause condition))
            (context (esrap::esrap-parse-error-context cause)))
       (esrap::error-report context stream)))))
