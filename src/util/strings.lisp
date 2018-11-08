;;;; strings.lisp --- String-related utilities.
;;;;
;;;; Copyright (C) 2015, 2016, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.util)

(defun safe-name (name)
  (substitute #\_ #\/ name))

;;; Edit distance

(declaim (ftype (function (string string
                                  &key
                                  (:window array-index)
                                  (:upper-bound array-index))
                          (values array-index &optional))
                edit-distance))
(defun edit-distance (string1 string2
                      &key
                      (upper-bound 2000)
                      (window      (truncate upper-bound (/ 1.2))))
  "Return the Levenshtein distance between STRING1 and STRING2.

   Based on:
   http://en.wikibooks.org/wiki/Algorithm_Implementation/Strings/Levenshtein_distance#Common_Lisp"
  (let ((n (length string1))
        (m (length string2)))
    ;; Check trivial cases
    (cond
      ((zerop n)
       m)
      ((zerop m)
       n)
      (t
       ;; We need to store only two columns---the current one that is
       ;; being built and the previous one.
       (let ((col      (make-array (1+ m) :element-type 'array-index))
             (prev-col (make-array (1+ m)
                                   :element-type     'array-index
                                   :initial-contents (iota (1+ m)))))
         (declare (type (simple-array array-index 1) col prev-col))
         ;; Loop across all chars of each string
         (locally (declare (optimize speed))
           (dotimes (i n)
             (setf (aref col 0) (1+ i))
             (let ((current
                     (iter (for (the (or (eql -1) array-index) j)
                                :from (max (- i window) 0)
                                :below (min (+ i window) m))
                           (let ((value (min (1+ (aref col j))
                                             (1+ (aref prev-col (1+ j)))
                                             (+ (aref prev-col j)
                                                (if (char= (schar string1 i)
                                                           (schar string2 j))
                                                    0
                                                    1)))))
                             (setf (aref col (1+ j)) value)
                             (minimizing (the array-index value))))))
               (declare (type array-index current))
               (when (> current upper-bound)
                 (return-from edit-distance upper-bound)))
             (fill col (- array-dimension-limit 2)
                   :start 0 :end (min (max (- i window) 0) (1+ m)))
             (fill col (- array-dimension-limit 2)
                   :start (min (+ i window 1) (1+ m)) :end (1+ m))
             (rotatef col prev-col))
           (aref prev-col (min m (+ n window -1)))))))))
