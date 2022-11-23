;;;; http.lisp --- Utilities for making HTTP requests.
;;;;
;;;; Copyright (C) 2019, 2022 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.api)

;;; `endpoint'

(defclass endpoint (utilities.print-items:print-items-mixin)
  ((%base-url    :initarg  :base-url
                 :reader   base-url
                 :initform (puri:uri "https://localhost:8080"))
   (%version     :initarg  :version
                 :accessor version
                 :initform nil)
   ;; Security
   (%credentials :initarg  :credentials
                 :reader   credentials
                 :initform nil)
   (%cookies     :initarg  :cookies
                 :reader   cookies
                 :initform (make-instance 'drakma:cookie-jar))
   (%csrf-token  :initarg  :csrf-token
                 :type     (or (eql nil) (eql t) (cons string string))
                 :accessor csrf-token
                 :initform nil)))

(defmethod print-items:print-items append ((object endpoint))
  (let ((credentials (when-let ((credentials (credentials object)))
                       (print-items:print-items credentials))))
    `(,@(when credentials
          `((:credentations                                         "~/print-items:format-print-items/" ,credentials)
            ((:separator (:after :credentials) (:before :base-url)) "@")))
      (:base-url                      "~A"                     ,(base-url object))
      ((:version (:after :base-url))  " version ~:[?~;~:*~A~]" ,(version object)))))

(defun make-endpoint (base-url &key credentials)
  (make-instance 'endpoint :base-url base-url :credentials credentials))

;;; Credentials

(defmethod credentials-initargs ((credentials null))
  '())

(defclass basic-credentials (print-items:print-items-mixin)
  ((%username :initarg  :username
              :reader   username)
   (%password :initarg  :password
              :reader   password)))

(defmethod print-items:print-items append ((object basic-credentials))
  `((:username                        "~A"       ,(username object))
    ((:separator (:after :username))  ":")
    ((:password  (:after :separator)) "********")))

(defmethod credentials-initargs ((credentials basic-credentials))
  (list :basic-authorization (list (username credentials)
                                   (password credentials))))

(defclass username+password-credentials (basic-credentials)
  ())

(defun make-username+password-credentials (username password)
  (make-instance 'username+password-credentials :username username
                                                :password password))

(defclass token-credentials (basic-credentials)
  ())

(defun make-token-credentials (username token)
  (make-instance 'token-credentials :username username :password token))

;;; Endpoint variable

(defvar *endpoint*)

(defun call-with-endpoint (thunk endpoint)
  (let ((*endpoint* endpoint))
    (funcall thunk)))

(defmacro with-endpoint ((endpoint) &body body)
  `(call-with-endpoint (lambda () ,@body) ,endpoint))

;;;

(defun checked-request (relative-url &rest args
                                     &key (endpoint     *endpoint*)
                                          (base-url     (base-url endpoint))
                                          (credentials  (credentials endpoint))
                                          (cookies      (cookies endpoint))
                                          (headers      '())
                                          (if-not-found #'error)
                                     &allow-other-keys)
  (let+ ((url     (puri:merge-uris relative-url base-url))
         (headers (let ((csrf-token (csrf-token endpoint)))
                    (if (consp csrf-token)
                        (list* csrf-token headers)
                        headers)))
         ((&whole values body code &rest &ign)
          (multiple-value-list
           (apply #'drakma:http-request url
                  :cookie-jar         cookies
                  :additional-headers headers
                  (append (credentials-initargs credentials)
                          (remove-from-plist
                           args
                           :endpoint :base-url :credentials :cookies
                           :headers :if-not-found))))))
    (cond ((member code '(404 410))
           (error-behavior-restart-case
               (if-not-found
                (object-not-found-error :endpoint     endpoint
                                        :relative-url url
                                        :code         code
                                        :body         body))))
          ((not (<= 200 code 399))
           (error 'request-failed-error :endpoint     endpoint
                                        :relative-url url
                                        :code         code
                                        :body         body))
          (t
           (values-list values)))))
