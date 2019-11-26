;;;; http.lisp --- Utilities for making HTTP requests.
;;;;
;;;; Copyright (C) 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.api)

(defun checked-request (url &rest args
                            &key (if-not-found #'error)
                                 (username     *username*)
                                 (password     *password*)
                            &allow-other-keys)
  (let+ (((&whole values body code &rest &ign)
          (multiple-value-list
           (apply #'drakma:http-request url
                  :cookie-jar *cookie-jar*
                  (append (when (and username password)
                            (list :basic-authorization
                                  (list username password)))
                          (remove-from-plist
                           args :if-not-found :username :password))))))
    (cond ((member code '(404 410))
           (error-behavior-restart-case
               (if-not-found
                (object-not-found-error :url url :code code :body body))))
          ((not (<= 200 code 399))
           (error 'request-failed-error :url url :code code :body body))
          (t
           (values-list values)))))
