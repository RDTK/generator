;;;; json.lisp --- Report analysis results.
;;;;
;;;; Copyright (C) 2015, 2016, 2017, 2018, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.report)

(defvar *platform-of-interest* '("ubuntu" "trusty" "x86_64"))

(defmethod report ((object sequence) (style (eql :json)) (target pathname))
  (map nil (rcurry #'report style target) object))

(defmethod report ((object project::distribution-spec)
                   (style  (eql :json))
                   (target pathname))
  (let ((name (util:safe-name (model:name object))))
    (ensure-directories-exist target)
    (with-output-to-file (stream (make-pathname :name     name
                                                :type     "json"
                                                :defaults target)
                                 :if-exists :supersede)
      (report object style stream))))

(defmethod report ((object project::distribution-spec)
                   (style  (eql :json))
                   (target stream))
  (json:with-object (target)
    (json:encode-object-member "name" (model:name object) target)
    (json:as-object-member ("access" target)
      (json:encode-json (model:access object) target))
    (json:as-object-member ("platform-requires" target)
      (json:encode-json (project:platform-requires object *platform-of-interest*) target))
    (report-variables object target)
    (let ((projects (remove-duplicates (mapcar #'model:parent
                                               (project:versions object)))))
      (json:as-object-member ("projects" target)
        (json:with-array (target)
          (with-sequence-progress (:report/json projects)
            (dolist (project projects)
              (progress "~/print-items:format-print-items/"
                        (print-items:print-items project))
              (json:as-array-member (target)
                (report project style target)))))))))

;; Describe project in separate file. Currently not used.
(defmethod report ((object project::project-spec)
                   (style  (eql :json))
                   (target pathname))
  (let ((directory (merge-pathnames "projects/" target))
        (name      (util:safe-name (model:name object))))
    (ensure-directories-exist directory)
    (with-output-to-file (stream (make-pathname :name     name
                                                :type     "json"
                                                :defaults directory)
                                 :if-exists :supersede)
      (report object style stream))))

(defmethod report ((object project::project-spec)
                   (style  (eql :json))
                   (target stream))
  (let ((implementation (model:implementation object)))
    (json:with-object (target)
      (json:encode-object-member "name" (model:name object) target)
      (json:as-object-member ("versions" target)
        (json:with-array (target)
          (dolist (version (project:versions object))
            (json:as-array-member (target)
              (report version style target)))))
      (report-variables implementation target))))

(defmethod report ((object project::version-spec)
                   (style  (eql :json))
                   (target stream))
  (let ((implementation (model:implementation object)))
    (json:with-object (target)
      (json:encode-object-member "name" (model:name object) target)
      (json:encode-object-member "access" (model:access object) target)
      (json:encode-object-member "requires" (project:direct-requires object) target)
      (json:encode-object-member "provides" (project:direct-provides object) target)
      (json:encode-object-member
       "platform-requires" (project:platform-requires object *platform-of-interest*) target)
      (json:as-object-member ("direct-dependencies" target)
        (json:with-array (target)
          (dolist (dependency (model:direct-dependencies implementation))
            (let ((parent (model:parent dependency)))
              (json:as-array-member (target)
                (json:with-array (target)
                  (json:encode-array-member (model:name parent) target)
                  (json:encode-array-member (model:name dependency) target)))))))
      (report-variables implementation target))))

;;; Utilities

(defun report-variables (object stream)
  (json:as-object-member ("variables" stream)
    (json:with-object (stream)
      (loop :for (key . raw) :in (remove-duplicates
                                  (var:variables object)
                                  :key #'car :from-end t)
         :do (json:as-object-member ((string-downcase key) stream)
               (json:with-object (stream)
                 ;; (json:encode-object-member "raw" raw stream)
                 (handler-case
                     (let ((value (var:value object key)))
                       (json:encode-object-member "value" value stream))
                   (error (condition)
                     (let ((error (princ-to-string condition)))
                       (json:encode-object-member "error" error stream))))))))))
