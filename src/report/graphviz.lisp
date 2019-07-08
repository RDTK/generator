;;;; graphviz.lisp --- Graph generation for projects and dependencies.
;;;;
;;;; Copyright (C) 2012-2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:build-generator.report)

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
                                           (object model:named-mixin))
  (let ((label `(:html ()
                  (:table ((:border "0"))
                    (:tr ()
                      (:td ()
                        ,(format nil "~/print-items:format-print-items/"
                                 (print-items:print-items object))))))))
   `(:label ,label)))

(defmethod graph-object-attributes append ((graph  jenkins-dependencies)
                                           (object project:version))
  `(:shape :box))

(defmethod graph-object-attributes append ((graph  jenkins-dependencies)
                                           (object project::platform-dependency))
  `(:shape :plain))

(defmethod cl-dot:graph-object-node ((graph  jenkins-dependencies)
                                     (object t))
  (make-instance 'cl-dot:node
                 :attributes (graph-object-attributes graph object)))

(defmethod cl-dot:graph-object-node ((graph  jenkins-dependencies)
                                     (object project:distribution))
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
                                          (object project:distribution))
  (project:versions object))

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
                                            (project::platform-dependency
                                             `(:color "orange"))
                                            (t
                                             '())))))))

  (defmethod cl-dot:graph-object-points-to ((graph  jenkins-dependencies)
                                            (object project:version))
    (let+ ((relations '())
           ((&flet add-relation (dependency provide)
              (push provide (cdr (or (assoc dependency relations :test #'eq)
                                     (let ((cell (cons dependency '())))
                                       (push cell relations)
                                       cell))))))
           (system (make-system)))
      ;; Resolve requirements using things provided by the
      ;; distribution.
      (map nil (lambda+ ((target . reasons))
                 (when-let ((target (case target
                                      ((nil)   (make-unsatisfied))
                                      (:system (when (eq object (jenkins-dependencies-root graph))
                                                 system))
                                      (t       target))))
                   (map nil (curry #'add-relation target) reasons)))
           (project:direct-dependencies/reasons object))
      ;; Add platform dependencies.
      (when (eq object (jenkins-dependencies-root graph))
        (map nil (lambda+ ((target . reasons))
                   (map nil (curry #'add-relation target) reasons))
             (project::direct-platform-dependencies/reasons object)))
      ;; Make and return edge descriptions.
      (mapcar (lambda+ ((target . specs))
                (dependency specs target))
              relations)))

  (defmethod cl-dot:graph-object-pointed-to-by ((graph  jenkins-dependencies)
                                                (object project:version))
    (when (eq object (jenkins-dependencies-root graph))
      (let ((provides (project:provides object)))
        (list (dependency provides (make-provided)))))))

;;; File generation

(defmethod object-filename ((object project:distribution))
  (util:safe-name (model:name object)))

(defmethod object-filename ((object project:version))
  (format nil "~A-~A"
          (util:safe-name (model:name (model:parent (model:specification object))))
          (util:safe-name (model:name object))))

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
  (let+ ((work '())
         ((&flet add-project-version (directory version)
            (push (cons directory version) work)))
         ((&flet add-distribution (distribution)
            (push (cons target distribution) work)
            (let ((directory (uiop:subpathname
                              target (object-filename distribution)
                              :type :directory)))
              (map nil (curry #'add-project-version directory)
                   (project:versions distribution))))))
    (cond ((emptyp object)
           nil)
          ((typep (first-elt object) 'project:distribution)
           (map nil #'add-distribution object))
          (t
           (error "Not handled")))
    (with-sequence-progress (:report/graph work)
      (lparallel:pmap
       nil (lambda+ ((directory . object))
             (progress "~/print-items:format-print-items/"
                       (print-items:print-items object))
             (with-simple-restart (continue "~@<Skip ~A report for ~A.~:>"
                                            style object)
               (more-conditions::without-progress
                 (report object style directory))))
       work))))
