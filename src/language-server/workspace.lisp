;;;; workspace.lisp --- TODO.
;;;;
;;;; Copyright (C) 2016, 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.language-server)

(defclass workspace (print-items:print-items-mixin)
  ((root-uri  :initarg  :root-uri
              :reader   root-uri)
   (root-path :initarg  :root-path
              :reader   root-path)
   (documents :reader   %documents
              :initform (make-hash-table :test #'equal)))
  (:default-initargs
   :root-uri  (error "missing required initarg :root-uri")
   :root-path (error "missing required initarg :root-path")))

(defmethod print-items:print-items append ((object workspace))
  `((:root-uri       ,(root-uri object))
    (:document-count ,(document-count object) " ~D document~:P"
                     ((:after :root-uri)))))

(defmethod document-count ((container workspace))
  (hash-table-count (%documents container)))

(defmethod find-document ((uri string) (container workspace))
  (gethash uri (%documents container)))

(defmethod (setf find-document) ((new-value t)
                                 (uri       string)
                                 (container workspace))
  (setf (gethash uri (%documents container)) new-value))

(defmethod (setf find-document) ((new-value (eql nil))
                                 (uri       string)
                                 (container workspace))
  (remhash uri (%documents container)))
