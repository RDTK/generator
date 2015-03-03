;;;; macros.lisp --- Macros provided by the model.aspects module.
;;;;
;;;; Copyright (C) 2012-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.model.aspects)

;;; `define-aspect' macro machinery

(defun parse-aspect-parameter-spec (aspect-name spec)
  ;; NAMES-AND-DEFAULT (i.e. binding name, variable name and default
  ;; value) are of one of the following forms:
  ;;
  ;;   NAME
  ;;   (NAME DEFAULT)
  ;;   ((BINDING-NAME PARAMETER-NAME) DEFAULT)
  ;;
  (let+ (((names-and-default &key (type t) documentation)
          (ensure-list spec))
         ((name &optional (default nil default-supplied?))
          (ensure-list names-and-default))
         ((variable &optional (name variable name-supplied?))
          (ensure-list name))
         (parameter-name (if name-supplied?
                            variable
                            (format-symbol :keyword "ASPECT.~A.~A"
                                           aspect-name name))))
    (values
     (apply #'make-instance 'aspect-parameter
            :variable     (make-variable-info
                           parameter-name type documentation)
            :binding-name name
            (cond
              ((not default-supplied?)
               '())
              ((equal default '(bail))
               (list :default-value ''%bail))
              (t
               (list :default-value default))))
     name-supplied?)))

(defun+ make-aspect-variable-form ((parameter . explicit-name?))
  (let+ (((&structure-r/o variable-info- name type documentation)
          (aspect-parameter-variable parameter)))
    (unless explicit-name?
      `(note-variable ',name ',type ,documentation))))

(defun make-aspect-parameter-form (parameter)
  (let+ (((&structure-r/o aspect-parameter-
                          variable binding-name
                          ((&values default-value default-value?)
                           default-value))
          parameter)
         (name (variable-info-name variable)))
    `(make-instance 'aspect-parameter
                    :variable     (check-variable-access
                                   ',name :if-undefined #'error)
                    :binding-name ',binding-name
                    ,@(when default-value?
                        `(:default-value ,default-value)))))

(defun make-aspect-extend!-body (aspect-var parameters body)
  `(catch '%bail
     (apply
      (lambda ,(mapcar #'aspect-parameter-binding-name parameters)
        ,@body)
      (aspect-process-parameters ,aspect-var))))

(defun make-aspect-form (name super-aspects parameters body
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
         (class-name (name->class-name name))
         ((&values parameters explicit-names)
          (loop :for spec :in parameters
             :for (parameter explicit-name?)
             = (multiple-value-list
                (parse-aspect-parameter-spec name spec))
             :collect parameter :into parameters
             :collect explicit-name? :into explicit-names
             :finally (return (values parameters explicit-names)))))
    `(progn
       ;; Announce variables at compile-time.
       (eval-when (:compile-toplevel :load-toplevel :execute)
         ,@(mapcar (compose #'make-aspect-variable-form #'cons)
                   parameters explicit-names))

       ;; Define and register the aspect.
       (defclass ,class-name (,@(mapcar #'name->class-name super-aspects)
                              aspect)
         ((parameters :type     list
                      :allocation :class
                      :reader   aspect-parameters
                      :initform (list ,@(mapcar #'make-aspect-parameter-form
                                                parameters))
                      :documentation
                      "Stores a list of parameters accepted by the aspect."))
         (:default-initargs
          ,@(when constraints
              `(:constraints ',constraints)))
         ,@(when documentation
             `((:documentation ,documentation))))

       (service-provider:register-provider/class
        'aspect ,(make-keyword name) :class ',class-name)

       ,@(when body
           `((defmethod extend! progn ((,job-var    jenkins.api:job)
                                       (,aspect-var ,class-name)
                                       (,spec-var   t #+actually job))
               (log:debug "Applying ~A to ~A" ,aspect-var ,job-var)
               ,(make-aspect-extend!-body
                 aspect-var parameters
                 `((macrolet
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
                     ,@body)))
               ,job-var)))

       ',class-name)))

(defmacro define-aspect ((name &key (job-var    'job)
                                    (aspect-var 'aspect)
                                    (spec-var   'spec)
                                    constraints)
                          super-aspects
                          parameters
                         &body body)
  "Define an aspect class named NAME with SUPER-ASPECTS."
  (let+ (((&values body &ign documentation)
          (parse-body body :documentation t)))
    (make-aspect-form name super-aspects parameters body
                      :job-var       job-var
                      :aspect-var    aspect-var
                      :spec-var      spec-var
                      :constraints   constraints
                      :documentation documentation)))
