;;;; asdf.lisp ---
;;;;
;;;; Copyright (C) 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.analysis)

(defmacro with-reset-source-registry ((directory) &body body)
  "TODO(jmoringe): document"
  (once-only (directory)
    `(progn
       (asdf:initialize-source-registry `(:source-registry
                                          :ignore-inherited-configuration
                                          (:tree ,,directory)))
       ,@body)))

(defmacro with-silenced-output (&body body)
  `(let* ((*standard-output* (make-broadcast-stream))
          (*error-output*    *standard-output*)
          (*trace-output*    *standard-output*)
          (*debug-io*        *standard-output*))
     ,@body))

(defun load-system (file)
  (let ((name      (pathname-name file))
        (directory (make-pathname :directory (pathname-directory file))))
    (handler-case
        (with-reset-source-registry (directory)
          (handler-bind ((warning #'muffle-warning))
            (progn #+no with-silenced-output
                   (asdf:clear-system name)
                   (let ((*load-truename* file))
                     (asdf::load-sysdef name file))
                   (asdf:find-system name))))
      #+sbcl (sb-c:compiler-error (condition)
               (error "~@<Could not load ~S: ~A~@:>"
                      file (ignore-errors (princ-to-string condition)))))))

(defmethod analyze ((file pathname)
                    (kind (eql :asdf/one-file))
                    &key)
  (let+ ((system (load-system file))
         (name (asdf:component-name system))
         (version (asdf:component-version system))
         ((&labels dependency-name (dependency)
            (etypecase dependency
              (cons (dependency-name (second dependency)))
              (t    (string-downcase dependency)))))
         ((&flet dependency->list (dependency)
            (etypecase dependency
              ((cons (eql :version))
               (list :asdf (string-downcase (second dependency))
                     (parse-version (third dependency))))
              (t
               (list :asdf (string-downcase dependency))))))
         (all-dependencies
          (remove-duplicates
           (asdf:component-sideway-dependencies system)
           :test #'string-equal :key #'dependency-name))
         (non-system-dependencies
          (remove-if (compose
                      (rcurry #'find (ql:system-list) :test #'string-equal :key #'ql-dist:name)
                      #'dependency-name)
                     all-dependencies)))
    (append
     (list :versions `((:main . ,version))
           :provides `((:asdf ,name ,(parse-version version)))
           :requires (mapcar #'dependency->list non-system-dependencies))
     (when (asdf:system-description system)
       `(:description ,(asdf:system-description system)))
     (when (asdf:system-author system)
       `(:authors (,(asdf:system-author system))))
     (when (asdf:system-maintainer system)
       `(:maintainers (,(asdf:system-maintainer system))))
     (when (asdf:system-license system)
       `(:license ,(asdf:system-license system))))))

(defmethod analyze ((directory pathname)
                    (kind      (eql :asdf))
                    &key)
  "TODO(jmoringe): document"
  ;;; TODO(jmoringe, 2013-03-11): not just first
  (first (iter (for file in (find-files (merge-pathnames "*.asd" directory)))
               (restart-case
                   (collect (analyze file :asdf/one-file))
                 (continue (&optional condition)
                   :report (lambda (stream)
                             (format stream "~@<Skip file ~S.~@:>"
                                     file))
                   (declare (ignore condition)))))))
