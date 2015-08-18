;;;; maven.lisp --- Analysis of maven projects.
;;;;
;;;; Copyright (C) 2013, 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.analysis)

;;; Constants

(define-constant +pom-namespace+
  "http://maven.apache.org/POM/4.0.0"
  :test #'string=)

;;;

(defvar *projects* '())

(defmethod analyze ((directory pathname)
                    (kind      (eql :maven))
                    &key)
  (let* ((maven-file (merge-pathnames "pom.xml" directory))
         (document   (cxml:parse maven-file (stp:make-builder))))
    (xloc:with-locations-r/o
        (((:val id :type 'list/dependency)           "project")
         ((:val id/parent :type 'list/dependency)    "project/parent"
                                                     :if-no-match :do-nothing)
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
         (modules                                    "project/modules/module/text()"
                                                     :if-multiple-matches :all)
         :namespaces `((nil . ,+pom-namespace+)))
        document
      (let+ ((id/merged    (if id/parent (merge-ids id id/parent) id))
             (name+version (id->name+version id/merged))
             (license      (or license (analyze directory :license)))
             (sub-provides '())
             (sub-requires '())
             ((&flet project-property (name prefix id)
                (when (starts-with-subseq prefix name)
                  (let ((name (subseq name (1+ (length prefix)))))
                    (cond
                      ((and (string= name "groupId")    (first id)))
                      ((and (string= name "artifactId") (second id)))
                      ((and (string= name "version")    (fourth id))))))))
             ((&flet+ property-value-in-project
                  (name (id/merged id/parent properties))
                (cond
                  ((when-let ((value (project-property
                                      name "parent" id/parent)))
                     (warn "~@<The property name \"~A\" is deprecated; ~
                            use \"project.~:*~A\" instead.~@:>"
                           name)
                     value))
                  ((project-property name "project.parent" id/parent))
                  ((project-property name "project"        id/merged))
                  ((cdr (assoc name properties :test #'string=))))))
             ((&flet property-value (name)
                (or (some (curry #'property-value-in-project name) *projects*)
                    (cerror "Use the empty value"
                            "~@<Could not resolve reference to ~
                             property ~S in (sub-)module hierarchy~
                             ~@:_~@:_~
                             ~2@T~@<~{~{~A ~A~}~^~@:_~}~:>~
                             ~@:_~@:_.~@:>"
                            name (reverse (mapcar (compose #'id->name+version #'first)
                                                  *projects*))))))
             (*projects* (list* (list id/merged id/parent properties)
                                *projects*))
             ((&flet process-sub-project (name)
                (let+ ((sub-directory (merge-pathnames (concatenate 'string name "/") directory))
                       ((&plist-r/o (provides :provides) (requires :requires))
                        (analyze sub-directory :maven)))
                  (unionf sub-provides provides :test #'equal)
                  (unionf sub-requires requires :test #'equal))))
             ((&flet+ process-dependency ((name version1))
                (list* :maven (%resolve-maven-value name #'property-value)
                       (when version1
                         (list (parse-version
                                (%resolve-maven-version
                                 version1 #'property-value))))))))
        ;; Analyse sub-modules, populating `sub-provides' and
        ;; `sub-requires'.
        (mapc #'process-sub-project modules)
        ;; Combine results for "main module" and sub-modules.
        (let ((provides `(,(process-dependency name+version)
                          ,@sub-provides))
              (requires `(,@(mapcar (compose #'process-dependency #'id->name+version)
                                    dependencies)
                          ,@sub-requires)))
          ;; Since sub-modules can depend on each other, remove
          ;; requirements that are provided by the project (including
          ;; sub-modules).
          (setf requires (set-difference
                          requires provides
                          :test (lambda+ ((mechanism1 name1 &optional version1)
                                          (mechanism2 name2 &optional version2))
                                  (and (eq              mechanism1 mechanism2)
                                       (string=         name1      name2)
                                       (version-matches version1   version2)))))
          (append (list :versions `((:main ,name+version)) ; TODO remove
                        :provides provides
                        :requires requires)
                  (when description `(:description ,description))
                  (when url         `(:url         ,url))
                  (when license     `(:license     ,license))
                  (when properties  `(:properties  ,properties))))))))

;;; Utility functions

(defun %resolve-maven-value (spec lookup)
  (let+ (((&labels replace1 (value &optional (depth 10))
            (when (zerop depth)
              (error "~@<Failed to expand property reference ~S~@:>"
                     spec))
            (let+ (((&values result match?)
                    (ppcre:regex-replace-all
                     "\\${([^${}]+)}" value
                     (lambda (expression name)
                       (declare (ignore expression))
                       (replace1 (funcall lookup name) (1- depth)))
                     :simple-calls t)))
              (if match? (replace1 result (1- depth)) result)))))
    (replace1 spec)))

(defun %parse-maven-version-spec (string)
  (or (ppcre:register-groups-bind (open version close)
          (#.(format nil "^(\\[|\\()?~
                           ([^]),]+)~
                           (?:,(?:[^]),]+)?)?~
                           (\\]|\\))?")
           string)
        (when (or (and open close) (not (or open close)))
          version))
      (error "~@<Invalid version specification: ~S.~@:>"
             string)))

(mapc (lambda+ ((input expected))
        (assert (equal expected (%parse-maven-version-spec input))))
      '(("1.0"           "1.0")
        ("[1.0)"         "1.0")
        ("(1.0)"         "1.0")
        ("[1.0,)"        "1.0")
        ("(1.0,)"        "1.0")
        ("[1.0,2.0)"     "1.0")
        ("(1.0,2.0)"     "1.0")
        ("[1.0]"         "1.0")
        ("(1.0]"         "1.0")
        ("[1.0,]"        "1.0")
        ("(1.0,]"        "1.0")
        ("[1.0,2.0]"     "1.0")
        ("(1.0,2.0]"     "1.0")
        ;; More ranges may follow.
        ("1.0,3.0"       "1.0")
        ("[1.0),3.0"     "1.0")
        ("(1.0),3.0"     "1.0")
        ("[1.0,),3.0"    "1.0")
        ("(1.0,),3.0"    "1.0")
        ("[1.0,2.0),3.0" "1.0")
        ("(1.0,2.0),3.0" "1.0")
        ("[1.0],3.0"     "1.0")
        ("(1.0],3.0"     "1.0")
        ("[1.0,],3.0"    "1.0")
        ("(1.0,],3.0"    "1.0")
        ("[1.0,2.0],3.0" "1.0")
        ("(1.0,2.0],3.0" "1.0")))

(defun %resolve-maven-version (spec lookup)
  (%parse-maven-version-spec (%resolve-maven-value spec lookup)))

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
  '(cons (or null string)
             (cons (or null string)
                   (cons (or null string)
                         (cons (or null string) null)))))

(defmethod xloc:xml-> ((value stp:element) (type (eql 'list/dependency))
                       &key
                       inner-types)
  (declare (ignore inner-types))
  (xloc:with-locations-r/o
      ((group   "groupId/text()"    :if-no-match :do-nothing) ; TODO which of these are actually optional?
       (name    "artifactId/text()" :if-no-match :do-nothing)
       (version "version/text()"    :if-no-match :do-nothing)
       #+not-used (type    "type/text()"       :if-no-match :do-nothing)
       (scope   "scope/text()"      :if-no-match :do-nothing)
       :namespaces `((nil . ,+pom-namespace+)))
      value
    (list group name scope version)))

(declaim (ftype (function (list/dependency) (values (cons string (cons (or null string) null)) &optional))
                id->name+version))
(defun+ id->name+version ((group name &ign #+later scope version))
  (list (format nil "~A/~A" #+later ~@[/~A~] group name #+later scope) version))

(declaim (ftype (function (list/dependency list/dependency)
                          (values list/dependency &optional))
                merge-ids))
(defun+ merge-ids ((&whole id1 group1 name1 scope1 version1)
                   (&whole id2 group2 name2 scope2 version2))
  (let+ (((&flet fail (component)
            (error "~@<None of the ids, ~A and ~A supplied for ~
                    merging, contain a ~S component.~@:>"
                   id1 id2 component))))
    (list (or group1   group2   (fail :group))
          (or name1    name2    (fail :name))
          (or scope1   scope2)
          (or version1 version2))))
