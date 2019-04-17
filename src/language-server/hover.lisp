;;;; hover.lisp --- Hover contributors for different contexts.
;;;;
;;;; Copyright (C) 2016, 2017, 2018, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.language-server)

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

(flet ((effective-value (object variable-name variable-location)
         (when object
           (values (format nil "```yaml~@
                                ~A~@
                                ```"
                           (handler-case
                               (var:value object variable-name)
                             (error (condition)
                               condition)))
                   variable-location
                   (format nil "Effective Value of `~(~A~)`" variable-name)))))

  (defmethod contrib:hover-contribution
      ((workspace   t)
       (document    t)
       (context     variable-value-context)
       (contributor effective-value-hover-contributor))
    (let+ (((&accessors-r/o object) document)
           ((&accessors-r/o variable-name variable-location) context))
      (effective-value object variable-name (sloc:range variable-location))))

  (defmethod contrib:hover-contribution
      ((workspace   t)
       (document    t)
       (context     variable-reference-context)
       (contributor effective-value-hover-contributor))
    (effective-value (object document)
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
     (context     system-package-name-context )
     (contributor system-package-name-hover-contributor))
  (when-let* ((packages (lparallel:force (ensure-platform-packages workspace)))
              (package  (find (word context) packages :test #'string= :key #'first)))
    (values (format nil "name: ~A~%version: ~A" (first package) (second package))
            (sloc:range (location context))
            "Package Information")))
