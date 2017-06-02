(cl:in-package #:jenkins.model.project)

(defun+ parse-dependency-spec ((nature name &optional version))
  (list* (make-keyword (string-upcase nature))
         name
         (etypecase version
           (list   version)
           (string (list (parse-version version))))))
