;;;; context.lisp --- TODO.
;;;;
;;;; Copyright (C) 2016, 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.language-server)

(defclass context () ; TODO maybe the workspace should store the documents?
  ((connection :initarg :connection)
   (documents  :reader   documents
               :initform (make-hash-table :test #'equal))))

(defmethod find-document ((uri string) (context context))
  (gethash uri (documents context)))

(defmethod (setf find-document) ((new-value t)
                                 (uri       string)
                                 (context   context))
  (setf (gethash uri (documents context)) new-value))

(defmethod (setf find-document) ((new-value (eql nil))
                                 (uri       string)
                                 (context   context))
  (remhash uri (documents context)))

;; TODO proper async stuff
(bt:make-thread
 (lambda ()
   (handler-case
       (json:decode-json-from-string (text object))
     (error (condition)
       ))))
