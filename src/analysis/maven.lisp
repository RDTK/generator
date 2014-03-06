;;;; maven.lisp --- Analysis of maven projects.
;;;;
;;;; Copyright (C) 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.analysis)

;;; Constants

(define-constant +pom-namespace+
  "http://maven.apache.org/POM/4.0.0"
  :test #'string=)

;;;

(defmethod analyze ((directory pathname)
                    (kind      (eql :maven))
                    &key)
  (let* ((maven-file (merge-pathnames "pom.xml" directory))
         (document (cxml:parse maven-file (stp:make-builder))))
    (xloc:with-locations-r/o
        ((name                                       "project/name/text()")
         ((:val version :type 'list/dependency)      "project")
         (description                                "project/description/text()"
                                                     :if-no-match :do-nothing)
         (url                                        "project/url/text()"
                                                     :if-no-match :do-nothing)
         (license                                    "licenses/license/name/text()"
                                                     :if-no-match :do-nothing)
         ((:val properties :type 'cons/property)     "project/properties/*"
          :if-multiple-matches :all)
         ((:val dependencies :type 'list/dependency) "project/dependencies/dependency"
          :if-multiple-matches :all)
         :namespaces `((nil . ,+pom-namespace+)))
        document
      (let+ ((license (or license (analyze directory :license)))
             ((&flet+ process-dependency ((kind name version))
                (list kind name
                      (parse-version (%resolve-maven-version version properties))))))
        (append
         (list :versions `((:main ,version)) ; TODO remove
               :provides `(,(process-dependency version))
               :requires (mapcar #'process-dependency dependencies))
         (when description `(:description ,description))
         (when url         `(:url         ,url))
         (when license     `(:license     ,license))
         (when properties  `(:properties  ,properties)))))))

;;; Utility functions

(defun %resolve-maven-value (spec properties)
  (let+ (((&flet lookup (name)
            (cdr (find name properties :key #'car :test #'string=))))
         ((&labels replace1 (value)
            (let+ (((&values result match?)
                    (ppcre:regex-replace-all
                     "\\${([^${}]+)}" value
                     (lambda (expression name)
                       (if-let ((value (lookup name)))
                         (replace1 value)
                         expression))
                     :simple-calls t)))
              (if match? (replace1 result) result)))))
    (replace1 spec)))

(defun %parse-maven-version-spec (string)
  (or (ppcre:register-groups-bind (open version close) ("(\\[|\\()?([^])]*)(\\]|\\))?" string)
        (when (or (and open close) (not (or open close)))
          version))
      (error "~@<Invalid version specification: ~S.~@:>"
             string)))

(defun %resolve-maven-version (spec properties)
  (%parse-maven-version-spec (%resolve-maven-value spec properties)))

;;; Conversion helpers

(deftype cons/property ()
  '(cons string string))

(defmethod xloc:xml-> ((value stp:element) (type (eql 'cons/property))
                       &key
                       inner-types)
  (declare (ignore inner-types))
  (xloc:with-locations-r/o (((:name name) ".")
                            (value        "text()")) value
    (cons name value)))

(deftype list/dependency ()
  '(cons string (cons string)))

(defmethod xloc:xml-> ((value stp:element) (type (eql 'list/dependency))
                       &key
                       inner-types)
  (declare (ignore inner-types))
  (xloc:with-locations-r/o ((group   "groupId/text()")
                            (name    "artifactId/text()")
                            (version "version/text()")
                            :namespaces `((nil . ,+pom-namespace+)))
      value
    (list :maven (format nil "~A/~A" group name) version)))
