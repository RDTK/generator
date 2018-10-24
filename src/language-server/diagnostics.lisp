;;;; diagnostics.lisp --- Diagnostics contributors for recipes.
;;;;
;;;; Copyright (C) 2018, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.DE>

(cl:in-package #:jenkins.language-server)

;;; Tabs

(defclass tabs-diagnostics-contributor () ())

(defmethod contrib:diagnostics-contributions
    ((workspace   t)
     (document    t)
     (contributor tabs-diagnostics-contributor))
  (loop :for i :from 0
        :for char :across (lsp:text document)
        :when (char= char #\Tab)
        :collect (sloc:make-annotation (sloc:make-location (source document) i (1+ i))
                                       "Don't use tabs"
                                       :kind :warning)))

;;;

(defclass variable-diagnostics-contributor () ())

(defmethod contrib:diagnostics-contributions
    ((workspace   t)
     (document    t)
     (contributor variable-diagnostics-contributor))
  (when-let ((object (object document)))
    (check-variables object document)
    #+TODO-not-that-easy (map nil (rcurry #'check-variables document)
         (project:aspects object))))

(defun check-variables (object document)
  (handler-case
      (mappend (named-lambda check-one (variable)
                 (let ((name (var:variable-info-name variable))
                       (type (var:variable-info-type variable)))
                   ;; HACK
                   (when (and (typep object 'project::distribution-spec)
                              (member name '(:jobs.list :jobs.dependencies
                                             :jobs.dependencies/groovy
                                             :platform-requires :natures
                                             :programming-languages :licenses
                                             :keywords)))
                     (return-from check-one))
                   (with-simple-restart
                       (continue "~@<Skip the variable ~A.~@:>" name)
                     (handler-bind
                         ((var:undefined-variable-error
                            #'continue)
                          (project::annotation-condition
                            (lambda (condition)
                              (lsp::debug1 (list :annotated-error name type condition))
                              (return-from check-one
                                (project::annotations condition))))
                          (error
                            (lambda (condition)
                              (lsp::debug1 (list :error name type condition))
                              (return-from check-one
                                (list (sloc:make-annotation
                                       (or (project::location-of
                                            (assoc name (var::variables object))
                                            (locations document))
                                           (sloc:make-location (source document) 0 1))
                                       (format nil "~@<Error in variable ~A: ~A~@:>"
                                               name condition)
                                       :kind :error))))))
                       (let+ (((&values value default?)
                               (var:value object name nil)))
                         (unless default?
                           (var:as value type)))
                       '()))))
               (var:all-variables))
    (error (condition)
      (lsp::debug1 condition)
      '())))
