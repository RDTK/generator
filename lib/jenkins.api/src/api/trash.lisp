
#+fixup-after-rename
(dolist (job (all-jobs))
  (let ((modified? nil))
    (dolist (renamed/old '("cca" "cca-oncilla" "rci" "rcirst"))
      (when (find (format nil "~A_pkg" renamed/old) (children job) :test #'string=)
	(setf (children job)
	      (substitute (format nil "~A-trunk_pkg" renamed/old)
			  (format nil "~A_pkg" renamed/old)
			  (children job)
			  :test #'string=))
	(setf modified? t)))
    (when modified?
      (print job)
      (commit! job))))
