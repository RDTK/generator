;;;; csrf.lisp --- Support Jenkins' CSRF protection.
;;;;
;;;; Copyright (C) 2017, 2018, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.api)

;;; CSRF

(defparameter *csrf-protection-token-url*
  (make-instance 'puri:uri
                 :path  "crumbIssuer/api/xml"
                 :query "xpath=concat(//crumbRequestField,\":\",//crumb)"))

(defun obtain-csrf-protection-token (&key (base-url *base-url*))
  (let ((url (puri:merge-uris *csrf-protection-token-url* base-url)))
    (log:info "Trying to obtain CSRF protection token from ~A" url)
    (more-conditions:with-condition-translation
        (((request-failed-error failed-to-obtain-csrf-token-error)
          :base-url base-url)
         ((error jenkins-connect-error)
          :base-url base-url))
      (let* ((result (checked-request url :if-not-found nil))
             (header (when result
                       (apply #'cons (split-sequence #\: result)))))
        (log:info "~@<Got cookies ~{~A~^, ~}~@:>"
                  (drakma:cookie-jar-cookies *cookie-jar*))
        (log:info "~@<~:[CSRF protection not enabled~;Got CSRF protection ~
                   token header ~:*~S~]~@:>"
                  header)
        header))))

(defvar *csrf-protection-token*)

(defun ensure-csrf-protection-token ()
  (if (boundp '*csrf-protection-token*)
      *csrf-protection-token*
      (setf *csrf-protection-token* (obtain-csrf-protection-token))))

;;; Jenkins version

(defun jenkins-version (&key (base-url *base-url*))
  (log:info "~@<Trying to contact Jenkins instance at ~A~@:>" base-url)
  (more-conditions:with-condition-translation
      (((error jenkins-connect-error) :base-url base-url))
    (let+ (((&values &ign &ign headers) (checked-request base-url))
           (version (assoc-value headers :x-jenkins)))
      (unless version
        (error "~@<Reply from Jenkins did not include X-Jenkins ~
                  header.~@:>"))
      (log:info "~@<Jenkins version is ~A~@:>" version)
      version)))

(defun verify-jenkins ()
  (prog1
      (jenkins-version)
    (ensure-csrf-protection-token)))
