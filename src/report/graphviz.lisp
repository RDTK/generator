;;;; graphviz.lisp --- Graph generation for projects and dependencies.
;;;;
;;;; Copyright (C) 2012-2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.report)

;;; Protocol

(defgeneric graph-object-attributes (graph object)
  (:method-combination append)
  (:documentation
   "Return a plist of attributes for the node representing OBJECT in
    GRAPH."))

(defgeneric object-filename (object)
  (:documentation
   "Return a suitable output filename for OBJECT."))

;;; jenkins.dependencies graph

(defstruct jenkins-dependencies
  (root nil))

(defmethod graph-object-attributes append ((graph  jenkins-dependencies)
                                           (object jenkins.model:named-mixin))
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
                                     (object jenkins.model.project::distribution-spec))
  nil)

(defstruct provided)

(defmethod cl-dot:graph-object-node ((graph  jenkins-dependencies)
                                     (object provided))
  (make-instance 'cl-dot:node :attributes `(:label "" :shape :none)))

(defstruct unsatisfied)

(defmethod cl-dot:graph-object-node ((graph  jenkins-dependencies)
                                     (object unsatisfied))
  (make-instance 'cl-dot:node :attributes `(:label "" :shape :none)))

(defstruct (system (:include unsatisfied)))

(defmethod cl-dot:graph-object-points-to ((graph  jenkins-dependencies)
                                          (object jenkins.model.project::distribution-spec))
  (mapcar #'jenkins.model:implementation (jenkins.model.project:versions object)))

(defmethod cl-dot:graph-object-points-to ((graph  jenkins-dependencies)
                                          (object jenkins.model.project::project))
  (jenkins.model.project:versions object))

(flet ((dependency (spec target)
         (let ((label (format nil "~:[?~:;~:*~{~{~(~A~):~A~^:~{~A~^.~}~}~^\\n~}~]"
                              spec)))
           (make-instance 'cl-dot:attributed
                          :object     target
                          :attributes `(:label ,label
                                        ,@(typecase target
                                            (provided
                                             `(:color     "green"
                                               :arrowhead :none
                                               :arrowtail :dot
                                               :dir       :both)) ; work around dot bug
                                            (system
                                             `(:color     "orange"
                                               :arrowhead :dot
                                               :arrowtail :none))
                                            (unsatisfied
                                             `(:color     "red"
                                               :arrowhead :dot
                                               :arrowtail :none))
                                            (t
                                             '())))))))

  (defmethod cl-dot:graph-object-points-to ((graph  jenkins-dependencies)
                                            (object jenkins.model.project::version))
    (let+ ((specification (jenkins.model:specification object))
           (requires      (jenkins.model.project:requires specification))
           (relations     '())
           ((&flet add-relation (dependency provide)
              (push provide (cdr (or (assoc dependency relations :test #'eq)
                                     (let ((cell (cons dependency '())))
                                       (push cell relations)
                                       cell))))))
           (system (make-system)))
      ;; Resolve requirements using things provided by the
      ;; distribution.
      (iter (for dependency in (remove-duplicates (jenkins.model:direct-dependencies object))) ; TODO how would there be duplicates?
            (let ((provides (remove (jenkins.model:specification dependency) requires
                                    :test-not #'eq
                                    :key      (rcurry #'jenkins.model.project:find-provider/version
                                                      :if-does-not-exist nil))))

              (iter (for provide in (or provides '(nil)))
                    (add-relation dependency provide))))
      ;; Resolve requirements using things provided by the platform.
      (mapc (lambda (requirement)
              (cond ((jenkins.model.project:find-provider/version
                      requirement :if-does-not-exist nil))
                    ((jenkins.model.project:find-provider/version
                      requirement
                      :providers         (jenkins.model.project:platform-provides object)
                      :if-does-not-exist nil)
                     (when (eq object (jenkins-dependencies-root graph))
                       (add-relation system requirement)))
                    (t
                     (add-relation (make-unsatisfied) requirement))))
            requires)
      ;; Make and return edge descriptions.
      (mapcar (lambda+ ((target . specs))
                (dependency specs target))
              relations)))

  (defmethod cl-dot:graph-object-pointed-to-by ((graph  jenkins-dependencies)
                                                (object jenkins.model.project::version))
    (let* ((specification (jenkins.model:specification object))
           (provides      (jenkins.model.project:provides specification)))
      (when (eq object (jenkins-dependencies-root graph))
        (list (dependency provides (make-provided)))))))

;;; File generation

(defmethod object-filename ((object jenkins.model.project::distribution-spec))
  (safe-name (name object)))

(defmethod object-filename ((object jenkins.model.project::version))
  (format nil "~A-~A"
          (safe-name (name (parent (specification object))))
          (safe-name (name object))))

(defmethod report ((object t) (style (eql :graph)) (target pathname))
  (ensure-directories-exist target)
  (let+ ((graph    (cl-dot:generate-graph-from-roots
                    (make-jenkins-dependencies :root object) (list object)))
         (basename (object-filename object))
         ((&flet filename (type)
            (make-pathname :name     basename
                           :type     type
                           :defaults target))))
    ;; Write dot output.
    (with-output-to-file (stream (filename "dot") :if-exists :supersede)
      (cl-dot:print-graph graph :stream stream))
    ;; Write PNG output.
    (cl-dot:dot-graph graph (filename "png") :format :png)))

;;; Entry points

(defmethod report ((object sequence) (style (eql :graph)) (target pathname))
  (if (length= 1 object) ; TODO hack for progress reporting
      (let ((element (first-elt object)))
        (with-simple-restart (continue "~@<Skip ~A report for ~A.~:>"
                                       style element)
          (list (report element style target))))
      (with-sequence-progress (:report/graph object)
        (lparallel:pmap
         nil (lambda (element)
               (progress "~/print-items:format-print-items/"
                         (print-items:print-items element))
               (with-simple-restart (continue "~@<Skip ~A report for ~A.~:>"
                                              style element)
                 (more-conditions::without-progress
                   (report element style target))))
         object))))

(defmethod report ((object jenkins.model.project::distribution-spec)
                   (style  (eql :graph))
                   (target pathname))
  ;; One graph for the distribution OBJECT as a whole.
  (call-next-method)
  ;; One graph for each project-version in OBJECT.
  (let ((directory (uiop:subpathname target (object-filename object)
                                     :type :directory)))
    (report (versions object) style directory)))

(defmethod report ((object jenkins.model.project::version-spec)
                   (style  (eql :graph))
                   (target pathname))
  (report (implementation object) style target))
