;;;; util.lisp --- Utilities used in the model module.
;;;;
;;;; Copyright (C) 2013, 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.model)

(define-condition cycle-error (error)
  ((path :initarg :path
         :reader  cycle-error-path))
  (:default-initargs
   :path (missing-required-initarg 'cycle-error :path))
  (:report
   (lambda (condition stream)
     (format stream "~@<The following elements form a cycle:~
                     ~@:_    ~{~A~^~@:_ -> ~}~@:>"
             (cycle-error-path condition)))))

(defstruct (node
            (:constructor make-node (object))
            (:predicate nil)
            (:copier nil))
  (object nil                                         :read-only t)
  (edges  '()  :type list)
  (state  :new :type (member :new :in-progress :done)))

(defun topological-sort (nodes)
  (let+ ((result '())
         ((&labels visit (node)
            (case (node-state node)
              (:in-progress
               (list node))
              (:new
               (setf (node-state node) :in-progress)
               (when-let ((cycle (some #'visit (node-edges node))))
                 (return-from visit (list* node cycle)))
               (setf (node-state node) :done)
               (push node result)
               nil))))
         ((&flet new? (node)
            (eq (node-state node) :new))))
    (loop :for node := (find-if #'new? nodes)
          :while node :do
          (when-let ((cycle (visit node)))
            (error 'cycle-error
                   :path (mapcar #'node-object cycle))))
    result))

(defun sort-with-partial-order (sequence predicate) ; TODO &key key
  (declare (type sequence sequence)
           (type function predicate))
  (let* ((nodes  (map '(simple-array t 1) #'make-node sequence))
         (length (length nodes)))
    ;; Build graph by checking PREDICATE for all pairs of distinct
    ;; elements of SEQUENCE.
    (loop :for i :below length
          :for node1   = (aref nodes i)
          :for object1 = (node-object node1) :do
          (loop :for j :from (1+ i) :below length
                :for node2   = (aref nodes j)
                :for object2 = (node-object node2) :do
                (when (funcall predicate object1 object2)
                  (pushnew node2 (node-edges node1) :test #'eq)) ; TODO for redundant cases
                (when (funcall predicate object2 object1)
                  (pushnew node1 (node-edges node2) :test #'eq))))
    ;; Topologically sort nodes and extract objects.
    (mapcar #'node-object (topological-sort nodes))))
