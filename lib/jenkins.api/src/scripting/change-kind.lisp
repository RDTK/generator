;;;; change-kind.lisp ---
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.scripting)

(when nil
 (dolist (job (all-jobs "-cl-(trunk|0\.[6-9])"))
   (when (string= (kind job) "project")
     (format t "~A ~A~%" (id job) (kind job))
     (setf (kind job) "matrix-project")
     (setf (slaves job) '("ubuntu_lucid_32bit" "ubuntu_lucid_64bit" "ubuntu_oneiric_32bit" "ubuntu_oneiric_64bit" "ubuntu_precise_32bit" "ubuntu_precise_64bit"))
     (setf (xloc:val (xloc:loc (stp:document-element (%data job)) "executionStrategy[@class=\"hudson.matrix.DefaultMatrixExecutionStrategyImpl\"]/runSequentially/text()" :if-no-match :create)) "false")
     (dolist (builder (builders job))
       (when (and (typep builder 'builder/shell)
		  (ppcre:scan "\\}/\\.\\./\\.\\./sbcl\\.sh" (command builder)))
	 (setf (command builder)
	       (ppcre:regex-replace "\\}/\\.\\./\\.\\./sbcl\\.sh" (command builder) "}/../../../../sbcl.sh"))
	 (format t "~2@T~A~%" (command builder))))
     (commit! job))))
