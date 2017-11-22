;;;; connection.lisp --- TODO.
;;;;
;;;; Copyright (C) 2016, 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.language-server)

(defclass connection ()
  ((input :initarg   :input
          :type      (satisfies input-stream-p)
          :reader    input)
   (output :initarg  :output
           :type     (satisfies output-stream-p)
           :reader   output))
  (:default-initargs
   :input  (error "missing initarg :input")
   :output (error "missing initarg :output")))

(defmethod read-request ((connection connection))
  (let+ ((raw       (transport/read-request (input connection)))
         (request   (json:decode-json-from-string raw))

         (id        (assoc-value request :id))
         (method    (assoc-value request :method))
         (arguments (alist-plist (assoc-value request :params))))
    (values id method arguments)))

(defmethod write-response ((connection connection) (id t) (payload t))
  (let ((raw (json:encode-json-to-string
              (make-response id `(:result . ,payload)))))
    (transport/write-response (output connection) raw)))

(defmethod write-response ((connection connection) (id t) (payload condition))
  (let* ((message (princ-to-string payload))
         (raw     (json:encode-json-to-string
                   (make-response
                    id `(:error  . ((:code    . 0) ; TODO code. search for "ErrorCodes" in spec
                                    (:message . ,message)))))))
    (transport/write-response (output connection) raw)))

(defmethod write-notification ((connection connection) (method string) (payload t))
  (let ((raw (json:encode-json-to-string
              `((:jsonrpc . "2.0")
                (:method  . ,method)
                (:params  . ,payload)))))
    (transport/write-response (make-broadcast-stream (output connection) *trace-output*) raw)))

;;; Utilities

(defun make-response (id body)
  `((:jsonrpc . "2.0")
    (:id      . ,id)
    ,body))
