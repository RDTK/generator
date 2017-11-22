(cl:in-package #:jenkins.language-server)

(defclass language-server-resolver ()
  ((workspace :initarg :workspace)))

(defmethod rs.f:resolve ((resolver language-server-resolver)
                         (format   t)
                         (location t)
                         &key
                           if-does-not-exist)
  (declare (ignore if-does-not-exist))
  nil)
