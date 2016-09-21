;;;; graphviz.lisp --- Graph generation for projects and dependencies.
;;;;
;;;; Copyright (C) 2012, 2013, 2014, 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.report)

;;; Protocol

(defgeneric graph-object-attributes (graph object)
  (:method-combination append)
  (:documentation
   "Return a plist of attributes for the node representing OBJECT in
    GRAPH."))

;;; jenkins.dependencies graph

(defmethod graph-object-attributes append ((graph  (eql :jenkins.dependencies))
                                           (object jenkins.project::named-mixin))
  (let ((label `(:html ()
                  (:table ((:border "0"))
                    (:tr ()
                      (:td ()
                        ,(format nil "~/print-items:format-print-items/"
                                 (print-items:print-items object))))))))
   `(:shape :box
     :label ,label)))

(defmethod cl-dot:graph-object-node ((graph  (eql :jenkins.dependencies))
                                     (object t))
  (make-instance 'cl-dot:node
                 :attributes (graph-object-attributes graph object)))

(defmethod cl-dot:graph-object-node ((graph  (eql :jenkins.dependencies))
                                     (object jenkins.project::distribution-spec))
  nil)

(defmethod cl-dot:graph-object-points-to ((graph  (eql :jenkins.dependencies))
                                          (object jenkins.project::distribution-spec))
  (mapcar #'jenkins.project:implementation (jenkins.project::versions object)))

(defmethod cl-dot:graph-object-points-to ((graph  (eql :jenkins.dependencies))
                                          (object jenkins.project::project))
  (jenkins.project::versions object))

(defmethod cl-dot:graph-object-points-to ((graph  (eql :jenkins.dependencies))
                                          (object jenkins.project::version))
  (let ((relations '()))
    (iter (for dependency in (remove-duplicates (jenkins.project::direct-dependencies object))) ; TODO how would there be duplicates?
          (let ((provides (remove (jenkins.project::specification dependency)
                                  (jenkins.project::requires (jenkins.project::specification object))
                                  :test-not #'eq
                                  :key      (rcurry #'jenkins.project::find-provider/version
                                                    :if-does-not-exist nil))))

            (iter (for provide in (or provides '(nil)))
                  (push provide (cdr (or (assoc dependency relations :test #'eq)
                                         (let ((cell (cons dependency '())))
                                           (push cell relations)
                                           cell)))))))
    (mapcar (lambda+ ((dependency . provides))
              (let ((label (format nil "~:[?~:;~:*~{~{~(~A~):~A~^:~{~A~^.~}~}~^\\n~}~]"
                                   provides)))
                (make-instance 'cl-dot:attributed
                               :object     dependency
                               :attributes `(:label ,label))))
            relations)))

(defmethod report ((object sequence) (style (eql :graph)) (target pathname))
  (with-sequence-progress (:report/graph object)
    (lparallel:pmap
     nil (lambda (element)
           (progress "~/print-items:format-print-items/"
                     (print-items:print-items element))
           (report element style target))
     object)))

(defmethod report ((object jenkins.project::distribution-spec)
                   (style  (eql :graph))
                   (target pathname))
  (call-next-method)
  (report (jenkins.project::versions object) style target))

(defmethod report ((object jenkins.project::version-spec)
                   (style  (eql :graph))
                   (target pathname))
  (report (jenkins.project::implementation object) style target))

(defmethod report ((object t)
                   (style  (eql :graph))
                   (target pathname))
  (ensure-directories-exist target)
  (let+ (((&flet safe-name (name)
            (substitute #\_ #\/ name)))
         (graph    (cl-dot:generate-graph-from-roots
                    :jenkins.dependencies (list object)))
         (basename (format nil "~@[~A-~]~A"
                           (when-let ((parent (when (compute-applicable-methods
                                                     #'jenkins.project::parent (list object))
                                                (jenkins.project::parent object))))
                             (safe-name (jenkins.project::name parent)))
                           (safe-name (jenkins.project::name object))))
         ((&flet filename (type)
            (make-pathname :name     basename
                           :type     type
                           :defaults target))))
    ;; Write dot output.
    (with-output-to-file (stream (filename "dot") :if-exists :supersede)
      (cl-dot:print-graph graph :stream stream))
    ;; Write PNG output.
    (cl-dot:dot-graph graph (filename "png") :format :png)))
