;;;; macros.lisp --- Macros provided by the model.aspects module.
;;;;
;;;; Copyright (C) 2012-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.model.aspects)

(defun make-aspect-class-form (name super-aspects variables body
                               &key
                                 (job-var    (when body
                                               (required-argument :job-var)))
                                 (spec-var   (when body
                                               (required-argument :spec-var)))
                                 (aspect-var (when body
                                               (required-argument :aspect-var)))
                                 constraints
                                 documentation)
  (let+ (((&flet name->class-name (name)
            (symbolicate '#:aspect- (string-upcase name))))
         (class-name (name->class-name name)))
    `(progn
       (defclass ,class-name (,@(mapcar #'name->class-name super-aspects) aspect) ()
         (:default-initargs
          ,@(when constraints
              `(:constraints ',constraints)))
         ,@(when documentation
             `((:documentation ,documentation))))

       (service-provider:register-provider/class
        'aspect ,(make-keyword name) :class ',class-name )

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
  (let+ (((&values body &ign documentation)
          (parse-body body :documentation t)))
    (make-aspect-class-form name super-aspects variables body
                            :job-var       job-var
                            :aspect-var    aspect-var
                            :spec-var      spec-var
                            :constraints   constraints
                            :documentation documentation)))
