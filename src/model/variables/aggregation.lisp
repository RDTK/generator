;;;; aggregation.lisp --- Aggregation of variable values.
;;;;
;;;; Copyright (C) 2012-2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.model.variables)

;;; Append strategy

(defmethod aggregate-values ((value    t)
                             (children t)
                             (name     t)
                             (strategy (eql :append)))
  (remove-duplicates
   (append value (mappend (lambda (version)
                            (value version name '()))
                          children))
   :test #'string=))

;;; Merge strategy

(defun merge-alists (left right &key (merge (rcurry #'union :test #'equal)))
  (let+ ((result (cons nil '()))
         ((&labels merge-cell (result-value-cell value)
            (setf (cdr result-value-cell)
                  (funcall merge (cdr result-value-cell) value))))
         ((&labels visit-cell (result-value-cell source-value)
            (typecase source-value
              ((cons (cons keyword t) list)
               (visit-alist result-value-cell source-value))
              (t
               (merge-cell result-value-cell source-value)))))
         ((&labels visit-alist (result-list-cell source-list)
            (map nil (lambda+ ((key . value))
                       (let ((result-value-cell
                               (or (assoc key (cdr result-list-cell))
                                   (let ((cell (cons key '())))
                                     (push cell (cdr result-list-cell))
                                     cell))))
                         (visit-cell result-value-cell value)))
                 source-list))))
    (visit-alist result left)
    (visit-alist result right)
    (cdr result)))

(assert
 (set-equal
  (merge-alists '((:a . (1 2))
                  (:c . ((:d . (7 8)) (:e . (9 10)))))
                '((:b . (3 4))
                  (:a . (5 6))
                  (:c . ((:e . (9 11))))))
  '((:a . (1 2 5 6))
    (:b . (3 4))
    (:c . ((:d . (7 8))
           (:e . (9 10 11)))))
  :test (named-lambda alist-tree-equal (left right)
          (and (eq (car left) (car right))
               (typecase (cdr left)
                 ((cons cons)
                  (set-equal (cdr left) (cdr right) :test #'alist-tree-equal))
                 (t
                  (set-equal (cdr left) (cdr right))))))))

(defmethod aggregate-values ((value    t)
                             (children t)
                             (name     t)
                             (strategy (eql :merge)))
  (reduce (lambda (merged version)
            (merge-alists merged (value version name '())))
          children :initial-value value))

;;; Histogram strategy

(defmethod aggregate-values ((value    t)
                             (children t)
                             (name     t)
                             (strategy (eql :histogram)))
  (let ((counts (make-hash-table :test #'eq)))
    (map nil (lambda (child)
               (map nil (lambda (value)
                          (incf (gethash (make-keyword value) counts 0)))
                    (value child name '())))
         children)
    (hash-table-alist counts)))
