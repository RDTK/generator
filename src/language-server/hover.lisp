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

(defmethod contrib:hover-contribution
    ((workspace   t)
     (document    t)
     (context     variable-value-context)
     (contributor effective-value-hover-contributor))
  (let+ (((&accessors-r/o object) document)
         ((&accessors-r/o variable-name variable-location) context))
    (when object
      (values (format nil "```yaml~@
                           ~A~@
                           ```"
                      (handler-case
                          (var:value object variable-name)
                        (error (condition)
                          condition)))
              (sloc:range variable-location)
              "Effective Value"))))

;;; Project version

(defclass project-version-hover-contributor () ())

(defmethod contrib:hover-contribution
    ((workspace   t)
     (document    t)
     (context     project-name-context)
     (contributor project-version-hover-contributor))
  (let ((prefix   (prefix context))
        (projects (projects (workspace document))))
    (when (lparallel:fulfilledp projects)
      (map nil (lambda (project)
                 (let ((name (model:name project)))
                   (when (starts-with-subseq prefix name)
                     (return-from contrib:hover-contribution
                       (values (describe-project project)
                               (sloc:range (location context)))))))
           (lparallel:force projects)))))

;;; System package name

(defclass system-package-name-hover-contributor () ())

(defmethod contrib:hover-contribution
    ((workspace   t)
     (document    t)
     (context     system-package-name-context )
     (contributor system-package-name-hover-contributor))
  (when-let* ((packages (lparallel:force (ensure-platform-packages workspace)))
              (package  (find (word context) packages :test #'string= :key #'first)))
    (values (format nil "name: ~A~%version: ~A" (first package) (second package))
            (sloc:range (location context))
            "Package Information")))
