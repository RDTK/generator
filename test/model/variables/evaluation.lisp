;;;; evaluation.lisp --- Unit tests for variable evaluation.
;;;;
;;;; Copyright (C) 2012-2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.model.variables.test)

(in-suite :jenkins.project.model.variables)

;;; Merge functions

(test merge-lookup-results.smoke
  "Smoke test for the `merge-lookup-results' function."

  (mapc
   (lambda+ ((left right expected))
     (is (equal expected (merge-lookup-results left right))))
   '(((nil () nil)
      (nil () nil)
      (nil () nil))

     (((:a . 1) ((:a . 2) (:a . 3)) t)
      (nil () nil)
      ((:a . 1) ((:a . 2) (:a . 3)) t))

     ((nil () nil)
      ((:a . 4) ((:a . 5) (:a . 6)) t)
      ((:a . 4) ((:a . 5) (:a . 6)) t))

     (((:a . 1) ((:a . 2) (:a . 3)) t)
      ((:a . 4) ((:a . 5) (:a . 6)) t)
      ((:a . 1) ((:a . 2) (:a . 3) (:a . 4) (:a . 5) (:a . 6)) t)))))


(test merge-lookup-values.smoke
  "Smoke test for the `merge-lookup-values' function."

  (is (equal (values nil () nil)
             (merge-lookup-values nil '() nil nil '() nil)))

  (is (equal (values '(:a . 1)
                     '((:a . 2) (:a . 3) (:a . 4) (:a . 5) (:a . 6))
                     t)
             (merge-lookup-values '(:a . 1) '((:a . 2) (:a . 3)) t
                                  '(:a . 4) '((:a . 5) (:a . 6)) t))))

(test merge-alists.smoke
  "Smoke test for the `merge-alists' function."

  (labels ((alist-tree-equal (left right)
             (and (eq (car left) (car right))
                  (typecase (cdr left)
                    ((cons cons)
                     (set-equal (cdr left) (cdr right) :test #'alist-tree-equal))
                    (t
                     (set-equal (cdr left) (cdr right))))))
           (set-equal/alist-tree-equal (left right)
             (set-equal left right :test #'alist-tree-equal)))
    (is (set-equal/alist-tree-equal
         '((:a . (1 2 5 6))
           (:b . (3 4))
           (:c . ((:d . (7 8))
                  (:e . (9 10 11)))))
         (merge-alists '((:a . (1 2))
                         (:c . ((:d . (7 8)) (:e . (9 10)))))
                       '((:b . (3 4))
                         (:a . (5 6))
                         (:c . ((:e . (9 11))))))))))

;;; Casts

(test as.smoke
  "Smoke test for the `as' generic function."

  (is (equal :system (as "system" '(or (eql :system) (eql :normal) string))))
  (is (equal :normal (as "normal" '(or (eql :system) (eql :normal) string))))
  (is (equal "foo"   (as "foo"    '(or (eql :system) (eql :normal) string)))))
