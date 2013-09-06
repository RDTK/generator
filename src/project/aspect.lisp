;;;; aspect.lisp --- Basic infrastructure for job aspects.
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.project)

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

                   (:after ASPECT-NAME)
                   (:before ASPECT-NAME)

                 where ASPECT-NAME designates the aspect to which this
                 aspect is related by the ordering constraint."))
  (:documentation
   "Instances of this class correspond to particular aspect of a
    generated build job such as checking out source code from a
    repository or parsing compiler messages and publishing a
    report."))

(defmethod aspect< ((left aspect) (right aspect))
  (let+ (((&accessors-r/o (constraints-left constraints))  left)
         ((&accessors-r/o (constraints-right constraints)) right)
         ((&flet+ satisfied? ((kind &rest args) other transpose?)
            (ecase kind
              (:after  (and transpose? (typep other (first args))))
              (:before (and (not transpose?) (typep other (first args))))))))
    (or (some (rcurry #'satisfied? right nil) constraints-left)
        (some (rcurry #'satisfied? left t)    constraints-right))))

(defmethod builder-constraints ((aspect t) (builder t))
  '())

(declaim (special *builder-constraints*))

(defvar *builder-constraints* nil
  "TODO(jmoringe): document")

(defmethod builder< ((left t) (right t) (constraints hash-table))
  (let+ (((&optional tag-left  constraints-left)  (gethash left constraints))
         ((&optional tag-right constraints-right) (gethash right constraints))
         ((&flet matches? (query thing)
            (or (eq query t) (eq query thing))))
         ((&flet+ satisfied? ((kind &rest args) other transpose?)
            (ecase kind
              (:after  (and transpose? (matches? (first args) other)))
              (:before (and (not transpose?) (matches? (first args) other)))))))
    (or (some (rcurry #'satisfied? tag-right nil) constraints-left)
        (some (rcurry #'satisfied? tag-left t)    constraints-right))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-aspect-class-form (name super-aspects variables body
                                 &key
                                 (job-var    (when body
                                               (required-argument :job-var)))
                                 (spec-var   (when body
                                               (required-argument :spec-var)))
                                 (aspect-var (when body
                                               (required-argument :aspect-var)))
                                 constraints)
   (let+ (((&flet name->class-name (name)
             (symbolicate '#:aspect- (string-upcase name))))
          (class-name (name->class-name name)))
     `(prog1
          (defclass ,class-name (,@(mapcar #'name->class-name super-aspects) aspect) ()
            (:default-initargs
             ,@(when constraints
                 `(:constraints ',constraints))))

        ,@(when variables
            `((defmethod variables append ((aspect ,class-name))
                (list ,@variables))))

        ,@(when body
            `((defmethod extend! progn ((,job-var    jenkins.api:job)
                                        (,aspect-var ,class-name)
                                        (,spec-var   t #+actually job))
                (log:debug "Applying ~A to ~A" ,aspect-var ,job-var)
                (flet ((var (name &optional (default nil default-supplied?))
                         (if default-supplied? ; TODO(jmoringe, 2013-03-04):
                             (handler-case
                                 (value ,aspect-var name)
                               (error ()
                                 default))
                             (value ,aspect-var name))))
                  (declare (ignorable #'var))
                  (macrolet
                      ((constraint! ((&optional constraints tag) &body builder)
                         `(let ((builder (progn ,@builder)))
                            (iter (for constraint in (append ',constraints
                                                             (builder-constraints ,',aspect-var builder)))
                                  (push constraint
                                        (second (ensure-gethash builder *builder-constraints*
                                                                (list ',(or tag ',name) '())))))
                            builder)))
                    ,@body))
                ,job-var)))))))

(defmacro define-aspect ((name &key (job-var    'job)
                                    (aspect-var 'aspect)
                                    (spec-var   'spec)
                                    constraints)
                         super-aspects
                         variables
                         &body body)
  "Define an aspect class named NAME with SUPER-ASPECTS."
  (make-aspect-class-form name super-aspects variables body
                          :job-var     job-var
                          :aspect-var  aspect-var
                          :spec-var    spec-var
                          :constraints constraints))
