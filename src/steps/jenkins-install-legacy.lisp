;;;; jenkins-install-legacy.lisp --- Steps for setting up a Jenkins instance.
;;;;
;;;; Copyright (C) 2015-2025 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:build-generator.steps)

(define-constant +default-jenkins-download-url+
    (puri:uri "https://archives.jenkins.io/war-stable/latest/jenkins.war")
  :test #'puri:uri=)

(defun jenkins-plugin-url (name &key (base-url +jenkins-plugins-base-url+))
  (puri:merge-uris (format nil "~A.hpi" name) base-url))

(define-constant +jenkins-plugin-manifest-filename+
  "META-INF/MANIFEST.MF"
  :test #'string=)

(defgeneric jenkins-plugin-dependencies (thing)
  (:method ((thing string))
    (let+ ((clean (ppcre:regex-replace-all
                   #.(format nil "~C~%(:? )?" #\Return) thing
                   (lambda (whole space)
                     (declare (ignore whole))
                     (if (emptyp space) (string #\Newline) ""))
                   :simple-calls t))
           ((&flet parse-dependency (spec)
              (ppcre:register-groups-bind (name version optional?)
                  ("([^:]+):([^;]+)(;resolution:=optional)?" spec)
                (list name version (when optional? t))))))
      (ppcre:register-groups-bind (dependencies)
          ("Plugin-Dependencies: +(.+)" clean)
        (mapcar #'parse-dependency
                (split-sequence:split-sequence #\, dependencies)))))
  (:method ((thing pathname))
    (jenkins-plugin-dependencies
     (zip:with-zipfile (zip thing)
       (let ((manifest (zip:get-zipfile-entry +jenkins-plugin-manifest-filename+ zip)))
         (sb-ext:octets-to-string (zip:zipfile-entry-contents manifest)))))))
