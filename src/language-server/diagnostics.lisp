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
                                         :natures
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
                          (map nil (lambda (annotation)
                                     (push (sloc:make-annotation
                                            (sloc:location annotation)
                                            (format nil "Error in variable ~A: ~?"
                                                    name
                                                    (simple-condition-format-control condition)
                                                    (simple-condition-format-arguments condition))
                                            :kind (sloc:kind annotation))
                                           result))
                               (project::annotations condition))
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
                          (continue)))) ; TODO use util:find-continue-restart
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

(defun check-dependency-value (value analysis-dependencies which)
  (when-let ((value (loop :for v :in value
                          :for parsed = (project::parse-dependency-spec v)
                          :do (when-let ((l (project::location-of v)))
                                (setf (project::location-of parsed) l))
                          :collect parsed)))
    (let* ((good (build-generator.analysis:effective-requires
                  value analysis-dependencies))
           (bad  (set-difference value good :test #'eq)))
      (loop :for offender :in bad
            :do (with-simple-restart (continue "~@<Ignore the error.~@:>")
                  (error "~@<The ~A ~A is included in the ~
                            automatic analysis results.~@:>"
                         (ecase which
                           (:requires "required feature")
                           (:provides "provided feature"))
                         offender))
                #+later (project::object-error (list (list
                                                      offender "specified here" :warn))
                                               "This requirement is redundant")))))

(defmethod check-variable-using-name ((object   t)
                                      (document project-document)
                                      (name     (eql :extra-requires))
                                      (info     t))
  (when-let* ((value        (call-next-method))
              (branches (handler-case
                            (var:value object :branches '())
                          (var:undefined-variable-error () nil)))
              (results  (first (analysis-results
                                (first branches) document :if-unavailable nil))))
    (unless (typep results 'condition)
      (check-dependency-value value (getf results :requires) :requires))))

(defmethod check-variable-using-name ((object   t)
                                      (document project-document)
                                      (name     (eql :extra-provides))
                                      (info     t))
  (when-let* ((value    (call-next-method))
              (branches (handler-case
                            (var:value object :branches '())
                          (var:undefined-variable-error () nil)))
              (results  (first (analysis-results
                                (first branches) document :if-unavailable nil))))
    (unless (typep results 'condition)
      (check-dependency-value value (getf results :provides) :provides))))

(defmethod check-variable-using-name ((object   t)
                                      (document project-document)
                                      (name     (eql :platform-requires))
                                      (info     t))
  (let+ (((&flet one-platform (platform)
            (let* ((direct    (loop :for value = (var:value object name '())
                                    :then (assoc-value value key)
                                    :for key-string :in platform
                                    :for key = (make-keyword (string-upcase key-string))
                                    :appending (assoc-value value :packages))))

              (let* ((results      (first (analysis-results "master" document :if-unavailable nil)))
                     (version-spec (make-instance 'project::version-spec
                                                  :name      "dummy"
                                                  :parent    object
                                                  :variables '()
                                                  :requires  (getf results :requires)))
                     (version      (make-instance 'project::version
                                                  :name          "dummy"
                                                  :specification version-spec
                                                  :jobs          '())))
                (handler-bind (((and error util:continuable-error) #'continue))
                  (model:add-dependencies! version :providers (make-hash-table)))
                (loop :with requires = (mappend (rcurry #'project::platform-requires platform)
                                                (project::direct-platform-dependencies version))
                      :for explicit :in direct
                      :when (and (find explicit requires :test #'string=)
                                 (if-let ((location (project::location-of explicit)))
                                   ;; TODO sources should be eq
                                   (equal (sloc:name (sloc:source location))
                                          (sloc:name (source document)))
                                   t))
                      :do (with-simple-restart (continue "Ignore the error")
                            (log:error explicit requires)
                            (project::object-error
                             (list (list explicit "specified here" :warning))
                             "The required package ~A is already ~
                              included in the automatically determined ~
                              analysis results."
                             explicit))))))))
    (one-platform '("ubuntu" "xenial"))))

(defmethod check-variable-using-name ((object   t)
                                      (document distribution-document)
                                      (name     (eql :platform-requires))
                                      (info     t))
  (let+ (((&flet one-platform (platform)
            (let* ((direct    (loop :for value = (var:value object name '())
                                    :then (assoc-value value key)
                                    :for key-string :in platform
                                    :for key = (make-keyword (string-upcase key-string))
                                    :appending (assoc-value value :packages)))
                   (offenders (make-hash-table :test #'equal)))
              (map nil (lambda (v)
                         (loop :for from-version :in (funcall (rcurry #'project:platform-requires platform) (project:version v))
                               :for name = (format nil "~A@~A"
                                                   (model:name (model:parent (project:version v)))
                                                   (model:name (project:version v)))
                               :for from-distribution = (find from-version direct :test #'string=)
                               :when from-distribution
                               :do (push name (gethash from-distribution offenders '()))))
                   (project:direct-versions object))
              offenders))))
    (let ((offenders (one-platform '("ubuntu" "xenial"))))
      (when (plusp (hash-table-count offenders))
        (maphash (lambda (item project-versions)
                   (with-simple-restart (continue "Ignore the error")
                     (project::object-error
                      (list (list item "specified here" :warning))
                      "The required package ~A is already specified in ~
                       the following included project versions:~@
                       ~{~A~^ ~}"
                      item project-versions)))
                 offenders)))))
