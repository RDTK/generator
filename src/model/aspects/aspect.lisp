;;;; aspect.lisp --- Basic infrastructure for job aspects.
;;;;
;;;; Copyright (C) 2012, 2013, 2014, 2015, 2016 Jan Moringen
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

(let+ (((&flet check-case (spec-a spec-b &optional no-relation?)
          (log:info spec-a spec-b)
          (let ((*builder-constraints* (make-hash-table)))
            (setf (gethash :a *builder-constraints*) spec-a
                  (gethash :b *builder-constraints*) spec-b)
            (if no-relation?
                (assert (not (builder< :a :b *builder-constraints*)))
                (assert (builder< :a :b *builder-constraints*)))
            (assert (not (builder< :b :a *builder-constraints*)))))))

  (check-case '(aspect-a "name-a" ((:before t)))
              '(aspect-b "name-b" ()))
  (check-case '(aspect-a "name-a" ((:before t)))
              '(aspect-b "name-b" ((:before t)))
              t)
  (check-case '(aspect-a "name-a" ((:before aspect-b)))
              '(aspect-b "name-b" ()))
  (check-case '(aspect-a "name-a" ((:before aspect-b)))
              '(aspect-b "name-b" ((:before t))))
  (check-case '(aspect-a "name-a" ((:before aspect-b "name-b")))
              '(aspect-b "name-b" ((:before aspect-a)))))

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
    `(progn
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
                        (apply #'value ,aspect-var name
                               (when default-supplied? (list default))))
                      (var/typed (name type &optional (default nil default-supplied?))
                        (as (apply #'value ,aspect-var name
                                   (when default-supplied? (list default)))
                            type)))
                 (declare (ignorable #'var))
                 (macrolet
                     ((constraint! ((&optional constraints tag) &body builder)
                        `(let* ((builder         (progn ,@builder))
                                (cell            (ensure-gethash
                                                  builder *builder-constraints*
                                                  (list ',(or tag ',name)
                                                        (name ,',aspect-var)
                                                        '())))
                                (all-constraints (append ',constraints
                                                         (builder-constraints
                                                          ,',aspect-var builder))))
                           (log:trace "~@<All constraints for ~A: ~:A~@:>"
                                      builder all-constraints)
                           (iter (for constraint in all-constraints)
                                 (push constraint (third cell)))
                           builder)))
                   ,@body))
               ,job-var)))

       ',class-name)))

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
