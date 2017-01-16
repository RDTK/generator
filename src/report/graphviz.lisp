;;;; graphviz.lisp --- Graph generation for projects and dependencies.
;;;;
;;;; Copyright (C) 2012-2017 Jan Moringen
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

(defstruct jenkins-dependencies
  (root nil))

(defmethod graph-object-attributes append ((graph  jenkins-dependencies)
                                           (object jenkins.project::named-mixin))
  (let ((label `(:html ()
                  (:table ((:border "0"))
                    (:tr ()
                      (:td ()
                        ,(format nil "~/print-items:format-print-items/"
                                 (print-items:print-items object))))))))
   `(:shape :box
     :label ,label)))

(defmethod cl-dot:graph-object-node ((graph  jenkins-dependencies)
                                     (object t))
  (make-instance 'cl-dot:node
                 :attributes (graph-object-attributes graph object)))

(defmethod cl-dot:graph-object-node ((graph  jenkins-dependencies)
                                     (object jenkins.project::distribution-spec))
  nil)

(defstruct provided)

(defmethod cl-dot:graph-object-node ((graph  jenkins-dependencies)
                                     (object provided))
  (make-instance 'cl-dot:node :attributes `(:label "" :shape :none)))

(defstruct unsatisfied)

(defmethod cl-dot:graph-object-node ((graph  jenkins-dependencies)
                                     (object unsatisfied))
  (make-instance 'cl-dot:node :attributes `(:label "" :shape :none)))

(defmethod cl-dot:graph-object-points-to ((graph  jenkins-dependencies)
                                          (object jenkins.project::distribution-spec))
  (mapcar #'jenkins.project:implementation (jenkins.project::versions object)))

(defmethod cl-dot:graph-object-points-to ((graph  jenkins-dependencies)
                                          (object jenkins.project::project))
  (jenkins.project::versions object))

(flet ((unresolved-depedency (spec kind)
         (let ((label (format nil "~{~(~A~):~A~^:~{~A~^.~}~}" spec)))
           (make-instance 'cl-dot:attributed
                          :object     (ecase kind
                                        (:provides (make-provided))
                                        (:requires (make-unsatisfied)))
                          :attributes `(:label ,label
                                        ,@(ecase kind
                                            (:provides
                                             `(:color     "green"
                                               :arrowhead :none
                                               :arrowtail :dot
                                               :dir       :both)) ; work around dot bug
                                            (:requires
                                             `(:color     "red"
                                               :arrowhead :dot
                                               :arrowtail :none))))))))

  (defmethod cl-dot:graph-object-points-to ((graph  jenkins-dependencies)
                                            (object jenkins.project::version))
    (let+ ((specification (jenkins.project::specification object))
           (requires      (jenkins.project::requires specification))
           (unsatisfied   (remove-if (rcurry #'jenkins.project::find-provider/version
                                             :if-does-not-exist nil)
                                     requires))
           (relations     '())
           ((&flet add-relation (dependency provide)
              (push provide (cdr (or (assoc dependency relations :test #'eq)
                                     (let ((cell (cons dependency '())))
                                       (push cell relations)
                                       cell)))))))
      (iter (for dependency in (remove-duplicates (jenkins.project::direct-dependencies object))) ; TODO how would there be duplicates?
            (let ((provides (remove (jenkins.project::specification dependency) requires
                                    :test-not #'eq
                                    :key      (rcurry #'jenkins.project::find-provider/version
                                                      :if-does-not-exist nil))))

              (iter (for provide in (or provides '(nil)))
                    (add-relation dependency provide))))

      (append
       (mapcar (lambda+ ((dependency . provides))
                 (let ((label (format nil "~:[?~:;~:*~{~{~(~A~):~A~^:~{~A~^.~}~}~^\\n~}~]"
                                      provides)))
                   (make-instance 'cl-dot:attributed
                                  :object     dependency
                                  :attributes `(:label ,label))))
               relations)
       (mapcar (rcurry #'unresolved-depedency :requires) unsatisfied))))

  (defmethod cl-dot:graph-object-pointed-to-by ((graph  jenkins-dependencies)
                                                (object jenkins.project::version))
    (let* ((specification (jenkins.project::specification object))
           (provides      (jenkins.project::provides specification)))
      (when (eq object (jenkins-dependencies-root graph))
        (mapcar (rcurry #'unresolved-depedency :provides) provides)))))

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
                    (make-jenkins-dependencies :root object) (list object)))
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
