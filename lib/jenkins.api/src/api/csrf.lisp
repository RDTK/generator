;;;; csrf.lisp --- Support Jenkins' CSRF protection.
;;;;
;;;; Copyright (C) 2017, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.api)

(defparameter *csrf-protection-token-url*
  (make-instance 'puri:uri
                 :path  "crumbIssuer/api/xml"
                 :query "xpath=concat(//crumbRequestField,\":\",//crumb)"))

(defun obtain-csrf-protection-token ()
  (let ((url (puri:merge-uris *csrf-protection-token-url* *base-url*)))
    (log:info "Trying to obtain CSRF protection token from ~A" url)
    (let+ (((&values result code)
            (apply #'drakma:http-request url
                   (when (and *username* *password*)
                     (list :basic-authorization
                           (list *username* *password*)))))
           (header (cond
                     ((<= 200 code 399)
                      (apply #'cons (split-sequence #\: result)))
                     ((<= 400 code 499)
                      nil)
                     (t
                      (error "~@<Failed to obtain CSRF protection token (code ~D): ~A~@:>"
                             code result)))))
      (log:info "~@<~:[CSRF protection not enabled~;Got CSRF protection ~
                 token header ~:*~S~]~@:>"
                header)
      header)))

(defvar *csrf-protection-token*)

(defun ensure-csrf-protection-token ()
  (if (boundp '*csrf-protection-token*)
      *csrf-protection-token*
      (setf *csrf-protection-token* (obtain-csrf-protection-token))))

(define-condition jenkins-connect-error (error
                                         more-conditions:chainable-condition)
  ((url :initarg :url
        :reader  url))
  (:default-initargs :url (missing-required-initarg 'jenkins-connect-error :url))
  (:report
   (lambda (condition stream)
     (format stream "~@<Could not connect to Jenkins instance at ~A.~
                     ~/more-conditions:maybe-print-cause/~@:>"
             (url condition) condition))))

(defun jenkins-version (&aux (url *base-url*) (username *username*) (password *password*))
  (log:info "~@<Trying to contact Jenkins instance at ~A~@:>" url)
  (more-conditions:with-condition-translation
      (((error jenkins-connect-error) :url url))
    (let+ (((&values body code headers)
            (apply #'drakma:http-request
                   url (when (and username password)
                         (list :basic-authorization
                               (list username password))))))
      (unless (<= 200 code 399)
        (error "~@<Request failed (code ~D):~_~A~@:>"
               code body))

      (let ((version (assoc-value headers :x-jenkins)))
        (unless version
          (error "~@<Reply from Jenkins did not include X-Jenkins ~
                  header.~@:>"))
        (log:info "~@<Jenkins version is ~A~@:>" version)
        version))))

(defun verify-jenkins ()
  (prog1
      (jenkins-version)
    (ensure-csrf-protection-token)))
