(cl:in-package :cxml-dom)

(defun adjust-vector-exponentially (vector new-dimension set-fill-pointer-p)
  (let ((d (array-dimension vector 0)))
    (when (< d new-dimension)
      (loop do (setf d (max 1 (* 2 d)))
         while (< d new-dimension))
      (adjust-array vector d))
    (when set-fill-pointer-p
      (setf (fill-pointer vector) new-dimension))))
