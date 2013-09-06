;;;; graphviz.lisp --- Graph generation for projects and dependencies.
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.project)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf who:*downcase-tokens-p*    nil
        who:*attribute-quote-char* #\"))

;;; Protocol

(defgeneric graph-object-attributes (graph object)
  (:method-combination append)
  (:documentation
   "TODO(jmoringe): document"))

(defgeneric graph-object-table-lines (object stream)
  (:method-combination progn)
  (:documentation
   "TODO(jmoringe): document"))

;;; jenkins.project graph

(defmethod graph-object-attributes append ((graph  (eql :jenkins.project))
                                           (object named-mixin))
  `(:shape :box
    ;; :label ,(name object)
    ))

(defmethod graph-object-table-lines progn ((object direct-variables-mixin)
                                           (stream t))
  (who:with-html-output (stream stream)
    (mapc (lambda+ ((key . value))
            (who:htm
             (:tr
              (:td (who:str key))
              (:td (let ((*print-pretty* nil))
                     (who:esc
                      (if (equal value (ignore-errors (value object key)))
                          (let ((string (prin1-to-string value)))
                            (subseq string 0 (min (length string) 40)))
                          (format nil "~S => ~S"
                                  (let ((string (princ-to-string value)))
                                    (subseq string 0 (min (length string) 40)))
                                  (let ((result (handler-case
                                                    (princ-to-string (value object key))
                                                  (error (condition)
                                                    (princ-to-string condition)))))
                                    (subseq result 0 (min (length result) 40)))))))))))
          (plist-alist (direct-variables object)))))

(defmethod graph-object-table-lines progn ((object version)
                                           (stream t))
  (who:with-html-output (stream stream)
    (mapc (lambda (name)
            (who:htm
             (:tr :colspan 2 :align "center"
               (:td :color "red" :align "center" name))))
          (dependencies object))))

(defmethod graph-object-attributes append ((graph  (eql :jenkins.project))
                                           (object direct-variables-mixin))
  `(:label ,(format
             nil "<~A>"
             (who:with-html-output-to-string (stream)
               (:table :border 0
                (:tr :colspan 2
                 (:td :align "center"
                  (who:str (or (ignore-errors (name object))
                               (class-name (class-of object))))))
                (graph-object-table-lines object stream))))))

(defmethod cl-dot:graph-object-node ((graph  (eql :jenkins.project))
                                     (object t))
  (make-instance 'cl-dot:node
                 :attributes (graph-object-attributes graph object)))

(defmethod cl-dot:graph-object-points-to ((graph  (eql :jenkins.project))
                                          (object project))
  (versions object))

(defmethod cl-dot:graph-object-points-to ((graph  (eql :jenkins.project))
                                          (object version))
  (jobs object))

(defmethod cl-dot:graph-object-points-to ((graph  (eql :jenkins.project))
                                          (object job))
  (aspects object)) ; TODO builders?

(defmethod cl-dot:graph-object-points-to ((graph  (eql :jenkins.project))
                                          (object aspect))
  (remove-if-not (rcurry #'aspect< object)
                 (remove object (aspects (parent object)))))

;;; jenkins.dependencies graph

(defmethod graph-object-attributes append ((graph  (eql :jenkins.dependencies))
                                           (object named-mixin))
  `(:shape :box
    :label ,(format nil "~/print-items:format-print-items/"
                    (print-items:print-items object))))

(defmethod cl-dot:graph-object-node ((graph  (eql :jenkins.dependencies))
                                     (object t))
  (make-instance 'cl-dot:node
                 :attributes (graph-object-attributes graph object)))

(defmethod cl-dot:graph-object-points-to ((graph  (eql :jenkins.dependencies))
                                          (object project))
  (versions object))

(defmethod cl-dot:graph-object-points-to ((graph  (eql :jenkins.dependencies))
                                          (object version))
  (iter outer
        (for dependency in (remove-duplicates (dependencies object))) ; TODO how would there be duplicates?
        (if-let ((providers (remove (specification dependency)
                                    (requires (specification object))
                                    :test-not #'eq
                                    :key      (rcurry #'find-provider/version
                                                      :if-does-not-exist nil))))
          (iter (for provider in providers)
                (in outer
                    (collect
                        (let ((label (format nil "~{~(~A~):~A~^:~{~A~^.~}~}" provider)))
                          (make-instance 'cl-dot:attributed
                                         :object     dependency
                                         :attributes `(:label ,label))))))
          (collect
              (make-instance 'cl-dot:attributed
                             :object     dependency
                             :attributes '(:label "?"))))))
