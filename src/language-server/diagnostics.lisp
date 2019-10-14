;;;; diagnostics.lisp --- Diagnostics contributors for recipes.
;;;;
;;;; Copyright (C) 2018, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.DE>

(cl:in-package #:build-generator.language-server)

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

;;; Diagnostics for variables

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
  (mappend (named-lambda check-one (variable)
             (let ((name   (var:variable-info-name variable))
                   (result '()))
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
                          (appendf result (project::annotations condition))
                          (continue)))
                      ((and error util:continuable-error)
                        (lambda (condition)
                          (push (sloc:make-annotation
                                 (or (project::location-of
                                      (assoc name (var::variables object))
                                      (locations document))
                                     (sloc:make-location (source document) 0 1))
                                 (format nil "~@<Error in variable ~A: ~A~@:>"
                                         name condition)
                                 :kind :error)
                                result)
                          (continue))))
                   (check-variable-using-name object document name variable)))
               result))
           (var:all-variables)))

(defmethod check-variable-using-name ((object   t)
                                      (document t)
                                      (name     t)
                                      (info     t))
  (let+ (((&values value default?) (var:value object name nil)))
    (unless default?
      (var:as value (var:variable-info-type info)))))

(defun check-dependency-value (value document which)
  (when-let* ((value   (loop :for v :in value
                             :for parsed = (project::parse-dependency-spec v)
                             :do (when-let ((l (project::location-of v)))
                                   (setf (project::location-of parsed) l))
                             :collect parsed))
              (results (when-let ((results (analysis-results
                                            document :if-unavailable nil)))
                         results)))
    (unless (typep results 'condition)
      (let* ((good (build-generator.analysis:effective-requires
                    value (getf (first results) which)))
             (bad  (set-difference value good :test #'eq)))
        (loop :for offender :in bad
              :do (with-simple-restart (continue "~@<Ignore the error.~@:>")
                    (error "~@<The requirement ~A is included in the ~
                          automatic analysis results.~@:>"
                           offender))
              #+later (project::object-error (list (list
                                                    offender "specified here" :warn))
                                             "This requirement is redundant"))))))

(defmethod check-variable-using-name ((object   t)
                                      (document project-document)
                                      (name     (eql :extra-requires))
                                      (info     t))
  (when-let ((value (call-next-method)))
    (check-dependency-value value document :requires)))

(defmethod check-variable-using-name ((object   t)
                                      (document project-document)
                                      (name     (eql :extra-provides))
                                      (info     t))
  (when-let ((value (call-next-method)))
    (check-dependency-value value document :provides)))

#+later (defmethod check-variable-using-name ((object   t)
                                      (document project-document)
                                      (name     (eql :platform-requires))
                                      (info     t))
  (project:platform-requires object '("ubuntu" "xenial"))
  (project::platform-))
