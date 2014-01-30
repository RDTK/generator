;;;; graph.lisp ---
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :jenkins.graph)

(declaim (special *cache*))

(defvar *cache* nil
  "TODO(jmoringe): document")

(defun %job (id)
  "TODO(jmoringe): document"
  (ensure-gethash id *cache* (job id)))

(declaim (special *predicate*))

(defvar *predicate* nil
  "TODO(jmoringe): document")

(defun make-graph (root-jobs
                   &key
                   (predicate (constantly t)))
  (let* ((*cache*     (make-hash-table :test #'equal))
         (unique      (mapcar (compose #'%job #'id) root-jobs))
         (*predicate* predicate))
    (cl-dot:generate-graph-from-roots 'jobs unique)))

(defun make-graph/file (jobs output-file
                        &rest args
                        &key
                        (format :png)
                        &allow-other-keys)
  "TODO(jmoringe): document"
  (cl-dot:dot-graph
   (apply #'make-graph jobs (remove-from-plist args :format))
   output-file
   :format   format
   :directed t))

(defmethod cl-dot:graph-object-node ((graph (eql 'jobs)) (job job))
  (make-instance 'cl-dot:node
                 :attributes (list :label (id job))))

(defmethod cl-dot:graph-object-pointed-to-by ((graph (eql 'jobs)) (job job))
  (remove-if (complement *predicate*) (mapcar #'%job (upstream job))))


#+for-example (progn
  (setf *job-cache* nil)
  (jenkins.graph:make-graph/file
   (all-jobs "0\\.7") "/tmp/0.7.png"
   :predicate (lambda (j) (ppcre:scan "0\\.7" (id j)))))
