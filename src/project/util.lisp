;;;; util.lisp --- Utilities used in the project module.
;;;;
;;;; Copyright (C) 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.project)

(defun sort-with-partial-order (list predicate)
  (iter (repeat (the integer (factorial (length list))))
        (unless
            (iter outer
                  (for i :below (length list))
                  (iter (for j :from (1+ i) :below (length list))
                        (when (funcall predicate (nth j list) (nth i list))
                          (rotatef (nth i list) (nth j list))
                          (return-from outer t))))
          (return-from sort-with-partial-order list)))
  (cerror "Continue without a valid order"
          "~<~S does not define a partial order on ~S.~@:>~%~
           Problems:~%~{* ~{~A - ~A~}~^~%~}"
          (list predicate list)
          (iter outer
                (for i :below (length list))
                (iter (for j :from (1+ i) :below (length list))
                      (when (funcall predicate (nth j list) (nth i list))
                        (in outer (collect (list (nth i list) (nth j list))))))))
  list)
