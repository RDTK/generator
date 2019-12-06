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

(defun obtain-csrf-protection-token (&key (endpoint *endpoint*))
  (log:info "Trying to obtain CSRF protection token from ~A" endpoint)
  (more-conditions:with-condition-translation
      (((request-failed-error failed-to-obtain-csrf-token-error)
        :endpoint     endpoint
        :relative-url *csrf-protection-token-url*)
       ((error jenkins-connect-error)
        :endpoint endpoint))
    (let* ((result (checked-request *csrf-protection-token-url*
                                    :endpoint     endpoint
                                    :if-not-found nil))
           (header (when result
                     (apply #'cons (split-sequence #\: result)))))
      (log:info "~@<Got cookies ~{~A~^, ~}~@:>"
                (drakma:cookie-jar-cookies (cookies endpoint)))
      (log:info "~@<~:[CSRF protection not enabled~;Got CSRF protection ~
                 token header ~:*~S~]~@:>"
                header)
      (or header t))))

(defun ensure-csrf-protection-token (&key (endpoint *endpoint*))
  (or (csrf-token endpoint)
      (setf (csrf-token endpoint)
            (obtain-csrf-protection-token :endpoint endpoint))))

;;; Jenkins version

(defun jenkins-version (&key (endpoint *endpoint*))
  (log:info "~@<Trying to contact Jenkins instance at ~A~@:>" endpoint)
  (more-conditions:with-condition-translation
      (((error jenkins-connect-error) :endpoint endpoint))
    (let+ (((&values &ign &ign headers) (checked-request "" :endpoint endpoint))
           (version (assoc-value headers :x-jenkins)))
      (unless version
        (error "~@<Reply from Jenkins did not include X-Jenkins ~
                  header.~@:>"))
      (log:info "~@<Jenkins version is ~A~@:>" version)
      version)))

(defun ensure-jenkins-version (&key (endpoint *endpoint*))
  (or (version endpoint)
      (setf (version endpoint) (jenkins-version :endpoint endpoint))))

(defun verify-jenkins (&key (endpoint *endpoint*))
  (ensure-csrf-protection-token :endpoint endpoint)
  (ensure-jenkins-version :endpoint endpoint))
