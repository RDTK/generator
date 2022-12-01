;;;; strings.lisp --- String-related utilities.
;;;;
;;;; Copyright (C) 2015-2022 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:build-generator.util)

(defun maybe-truncate (string)
  (let* ((string (string-left-trim '(#\Space #\Tab #\Newline) string))
         (length (length string))
         (end    (min (or (position #\Newline string) length) 30)))
    (if (> length end)
        (values (subseq string 0 end) t)
        (values string                nil))))

(defun safe-name (name)
  (substitute #\_ #\/ name))

;;; Edit distance

(declaim (ftype (function (string array-index string array-index
                           array-index array-index)
                          (values array-index &optional))
                %edit-distance))
(defun %edit-distance (string1 n string2 m upper-bound window)
  ;; We need to store only two columns -- the current one that is
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
            (return-from %edit-distance upper-bound))
          (fill col (- array-dimension-limit 2)
                :start 0 :end (min (max (- i window) 0) (1+ m)))
          (fill col (- array-dimension-limit 2)
                :start (min (+ i window 1) (1+ m)) :end (1+ m)))
        (rotatef col prev-col))
      (aref prev-col (min m (+ n window -1))))))

(declaim (ftype (function (string string
                                  &key
                                  (:window array-index)
                                  (:upper-bound array-index))
                          (values array-index &optional))
                edit-distance))
(defun edit-distance (string1 string2
                      &key
                      (upper-bound 2000)
                      (window      (max (truncate upper-bound (/ 1.2)) 1)))
  "Return the Levenshtein distance between STRING1 and STRING2.

   Based on:
   http://en.wikibooks.org/wiki/Algorithm_Implementation/Strings/Levenshtein_distance#Common_Lisp"
  (let ((n (length string1))
        (m (length string2)))
    ;; Check trivial cases
    (cond ((zerop n)
           m)
          ((zerop m)
           n)
          ((> (abs (- n m)) upper-bound)
           upper-bound)
          (t
           (%edit-distance string1 n string2 m upper-bound window)))))

(defun closest-matches (thing sequence &key count (limit 2) key)
  "Return a list of the COUNT most similar to THING elements of SEQUENCE.

   LIMIT controls the maximum number of differences compared to THING
   an element in SEQUENCE can have to still be potentially (since
   COUNT) included in the returned list.

   COUNT, if supplied, limits the number of returned elements.

   KEY, if supplied, has to be a designator of a function that is
   applied to each element of SEQUENCE to obtain a value that is
   compared to THING instead of the respective element."
  (let* ((string1    thing)
         (candidates (loop :for element :in sequence
                           :for string2 = (if key
                                              (funcall key element)
                                              element)
                           :for distance = (edit-distance
                                            string1 string2
                                            :upper-bound (+ limit 2))
                           :when (<= distance limit)
                             :collect (cons distance element)))
         (sorted     (sort candidates #'< :key #'car)))
    (when (and count (< count (length sorted)))
      (setf (cdr (nthcdr (1- count) sorted)) '()))
    (map 'list #'cdr sorted)))
