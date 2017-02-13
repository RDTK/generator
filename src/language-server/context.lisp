(cl:in-package #:jenkins.language-server)

(defclass context ()
  ((documents :reader   documents
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
