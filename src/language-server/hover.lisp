;;;; hover.lisp --- Hover contributors for different contexts.
;;;;
;;;; Copyright (C) 2016, 2017, 2018, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:build-generator.language-server)

;;; Variable information

(defclass variable-hover-contributor () ())

(defmethod contrib:hover-contribution
    ((workspace   t)
     (document    t)
     (context     known-variable-value-context)
     (contributor variable-hover-contributor))
  (let+ (((&accessors-r/o variable-location variable-node) context))
    (values (format nil "Type: `~A`~@
                         ~@
                         ~:[«undocumented»~;~:*~A~]"
                    (var:variable-info-type variable-node)
                    (var:variable-info-documentation variable-node))
            (sloc:range variable-location)
            "Variable Information")))

;;; Effective value

(defclass effective-value-hover-contributor () ())

(flet ((effective-value (container variable-name variable-location)
         (when container
           (values (format nil "```yaml~@
                                ~A~@
                                ```"
                           (handler-case
                               (var:value container variable-name)
                             (error (condition)
                               condition)))
                   variable-location
                   (format nil "Effective Value of `~(~A~)` in `~/print-items:format-print-items/`"
                           variable-name (print-items:print-items container))))))

  (defmethod contrib:hover-contribution
      ((workspace   t)
       (document    t)
       (context     variable-value-context)
       (contributor effective-value-hover-contributor))
    (let+ (((&accessors-r/o container variable-name variable-location) context))
      (effective-value container variable-name (sloc:range variable-location))))

  (defmethod contrib:hover-contribution
      ((workspace   t)
       (document    t)
       (context     variable-reference-context)
       (contributor effective-value-hover-contributor))
    (effective-value (container context)
                     (make-keyword (string-upcase (variable-name context))) ; TODO
                     (prefix-range context))))

;;; Project version

(defclass project-version-hover-contributor () ())

(defmethod contrib:hover-contribution
    ((workspace   t)
     (document    t)
     (context     project-name-context)
     (contributor project-version-hover-contributor))
  (when-let* ((prefix   (prefix context))
              (project (find-project prefix workspace)))
    (values (describe-project project) (sloc:range (location context)))))

;; TODO project-version-context

;;; System package name

(defclass system-package-name-hover-contributor () ())

(defmethod contrib:hover-contribution
    ((workspace   t)
     (document    t)
     (context     system-package-name-context)
     (contributor system-package-name-hover-contributor))
  (when-let* ((packages (lparallel:force (ensure-platform-packages workspace)))
              (package  (find (word context) packages :test #'string= :key #'first)))
    (values (format nil "name: ~A~%version: ~A" (first package) (second package))
            (sloc:range (location context))
            "Package Information")))

;;; Effective platform requirements contributor

(defclass effective-platform-requirements-contributor () ())

(defmethod contrib:hover-contribution
    ((workspace   t)
     (document    project-document)
     (context     known-variable-value-context)
     (contributor effective-platform-requirements-contributor))
  (when-let ((variable  (variable-node context))
             (container (container context)))
    (when (eq (var:variable-info-name variable) :platform-requires)
      (progn ; handler-bind ((error #'invoke-debugger))
        (let* ((results      (first (analysis-results "master" document :if-unavailable nil)))
               (version-spec (make-instance 'project::version-spec
                                            :name      "dummy"
                                            :parent    container
                                            :variables '()
                                            :requires  (getf results :requires)))
               (version      (make-instance 'project::version
                                            :name          "dummy"
                                            :specification version-spec
                                            :jobs          '())))
          (handler-bind (((and error util:continuable-error) #'continue))
            (model:add-dependencies! version :providers (make-hash-table))
            ; (clouseau:inspect version)
            (values (princ-to-string (project:platform-requires version '("ubuntu" "xenial")))
                    (sloc:range (location context))
                    "Effective Platform Requirements")))))))

;;; Analysis results

(defclass analysis-results-hover-contributor () ())

(defmethod contrib:hover-contribution
    ((workspace   t)
     (document    project-document)
     (context     known-variable-value-context)
     (contributor analysis-results-hover-contributor))
  (when-let ((variable (variable-node context))
             (project  (object document)))
    (case (var:variable-info-name variable)
      (:repository (analyze-1 document context (first (var:value project :branches))))
      (:branches   (analyze-1 document context (word context)
                              :fallback (first (var:value project :branches)))))))

(defun analyze-1 (document context branch &key fallback)
  (let+ (((&values body title)
          (let ((existing (or (analysis-results branch document :if-unavailable nil)
                              (when fallback
                                (analysis-results fallback document :if-unavailable nil)))))
            (cond ((null existing)
                   #+no (setf (analysis-results document)
                              (lparallel:task-handler-bind
                                  ((error #'lparallel:invoke-transfer-error))
                                (lparallel:future
                                  (multiple-value-list
                                   (maybe-analyze (object document) branch)))))
                   (values "*pending*" "Repository not yet analyzed"))
                  ((typep existing 'condition)
                   (values (format nil "```~%~A~%```" existing)
                           "Failed to analyze repository"))
                  (t #+no (lparallel:fulfilledp existing)
                     (handler-case
                         (let+ (((results branch natures) existing))
                           (values (format-analysis-results results)
                                   (format nil "Analysis results for branch `~A` with nature~P ~{`~A`~^, ~}"
                                           branch (length natures) natures)))
                       (error (condition)
                         (values (format nil "```~%~A~%```" condition)
                                 "Failed to analyze repository"))))
                  #+no (t
                        (values "*pending*" "Analyzing repository"))))))
    (values body (sloc:range (location context)) title)))

(defun maybe-analyze (object branch)
  )

(defun format-analysis-results (results)
  (with-output-to-string (stream)
    (loop :for (key value next) :on results :by #'cddr
          :do (format stream "**~A:**" key)
              (typecase value
                (cons
                 (format stream "~%~2@T~@<~@;~{* `~A`~^~%~}~:>" value))
                (string
                 (if (find #\Newline value)
                     (format stream "~%~2@T~@<~@;~{~A~^ ~}~@:>"
                             (split-sequence:split-sequence #\Space value)) ; TODO do something for filling
                     (format stream " `~A`" value)))
                (t
                 (format stream " `~A`" value)))
          :when next
          :do (terpri stream))))
