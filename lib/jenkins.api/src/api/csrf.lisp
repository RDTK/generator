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
