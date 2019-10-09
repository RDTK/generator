;;;; json.lisp --- Report analysis results.
;;;;
;;;; Copyright (C) 2015, 2016, 2017, 2018, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:build-generator.report)

(defvar *platform-of-interest* '("ubuntu" "trusty" "x86_64"))

(defmethod report ((object sequence) (style (eql :json)) (target pathname))
  (map nil (rcurry #'report style target) object))

(defmethod report ((object project:distribution)
                   (style  (eql :json))
                   (target pathname))
  (let ((name (util:safe-name (model:name object))))
    (ensure-directories-exist target)
    (with-output-to-file (stream (make-pathname :name     name
                                                :type     "json"
                                                :defaults target)
                                 :if-exists :supersede)
      (report object style stream))))

(defmethod report ((object project:distribution)
                   (style  (eql :json))
                   (target stream))
  (json:with-object (target)
    (json:encode-object-member "name" (model:name object) target)
    (json:as-object-member ("access" target)
      (json:encode-json (model:access object) target))
    (json:as-object-member ("platform-requires" target)
      (json:encode-json (project:platform-requires object *platform-of-interest*) target))
    (report-variables (model:specification object) target)
    (let ((project-versions (project:versions object)))
      (json:as-object-member ("project-versions" target)
        (json:with-array (target)
          (with-sequence-progress (:report/json project-versions)
            (dolist (project-version project-versions)
              (progress "~/print-items:format-print-items/"
                        (print-items:print-items project-version))
              (json:as-array-member (target)
                (report project-version style target)))))))))

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

(defmethod report ((object project:version)
                   (style  (eql :json))
                   (target stream))
  (let ((specification (model:specification object)))
    (json:with-object (target)
      (json:encode-object-member "project-name" (model:name (model:parent specification)) target)
      (json:encode-object-member "version-name" (model:name object) target)
      (json:encode-object-member "access" (model:access object) target)
      (json:encode-object-member "requires" (project:requires specification) target)
      (json:encode-object-member "provides" (project:provides specification) target)
      (json:encode-object-member
       "platform-requires" (project:platform-requires object *platform-of-interest*) target)
      (json:as-object-member ("direct-dependencies" target)
        (json:with-array (target)
          (dolist (dependency (model:direct-dependencies object))
            (let* ((specification (model:specification dependency))
                   (parent        (model:parent specification)))
              (json:as-array-member (target)
                (json:with-array (target)
                  (json:encode-array-member (model:name parent)        target)
                  (json:encode-array-member (model:name specification) target)))))))
      (report-variables specification target))))

;;; Utilities

(defun report-variables (object stream)
  (json:as-object-member ("variables" stream)
    (json:with-object (stream)
      (loop :for (key . raw) :in (remove-duplicates (var:direct-variables object)
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
