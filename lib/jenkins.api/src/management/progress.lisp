(defclass progress-reporter ()
  ((expected :initarg  :expected
	     :type     non-negative-integer
	     :reader   progress-reporter-expected
	     :documentation
	     ""))
  (:documentation
   "TODO(jmoringe): document"))

(defmethod xcvb-driver:slurp-input-stream ((reporter progress-reporter) (stream stream)
					   &key)
  (let ((dots 0))
    (iter (for line in-stream stream :using #'read-line)
	  (if-let ((progress (parse-integer line :junk-allowed t)))
	    (progn
	      (setf dots 0)
	      (print (float (/ progress (progress-reporter-expected reporter)))))
	    (progn
	      (when (zerop (mod (incf dots) 100))
		(princ ".")
		(force-output *standard-output*))
	      (when (= dots 7000)
		(terpri *standard-output*)
		(setf dots 0)))))))
