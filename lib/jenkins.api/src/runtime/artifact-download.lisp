;;;; artifact-download.lisp ---
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.runtime)

;;; TODO(jmoringe): should not be part of runtime

(defun download-job-artifact (job-name artifact-name destination
			      &key
			      (base-url *jenkins-base-url*)
			      (build    "lastSuccessfulBuild")
			      (error?   t))
  "TODO(jmoringe): document"
  (let ((url (format nil "~A/job/~A/~A/artifact/~A"
		     base-url job-name build artifact-name)))
    (alexandria:with-output-to-file
	(stream destination
		:if-does-not-exist :create
		:if-exists         :supersede
		:element-type      '(unsigned-byte 8))
      (format t "~&~@<; ~@;fetching ~S~@:>" url)
      (multiple-value-bind (http-stream result)
	  (drakma:http-request url :want-stream t)
	(cond
	  ((= result 200)
	   (alexandria:copy-stream http-stream stream
				   :element-type '(unsigned-byte 8))
	   destination)

	  (error?
	   (error "~@<Failed to download ~A ~A into ~A; Download failed with code ~D.~@:>"
		  job-name artifact-name destination result))

	  (t
	   nil))))))

(defun %job-names-for-system-name (name)
  (if (alexandria:starts-with-subseq "cl-" name)
      (list name (format nil "~A-cl" (subseq name 3)))
      (list name)))

(defun obtain-project-artifact (name destination-directory
				&key
				label
				suffixes
				(error?  t))
  (let ((combinations (alexandria:map-product #'list
					      (%job-names-for-system-name name)
					      (list label nil)
					      (append suffixes (list nil))))
	(destination  (make-pathname
		       :name     (concatenate 'string name ".tar")
		       :type     "gz"
		       :defaults destination-directory)))
    (flet ((try-combination (job-name label suffix)
	     (let ((job-part      (format nil "~A~@[-~A~]~@[/label=~A~]"
					  job-name suffix label))
		   (artifact-part (format nil "~A~@[-~A~].tar.gz"
					  job-name suffix)))
	       (and (download-job-artifact job-part artifact-part destination
					   :error? nil)
		    (%extract-archive destination)))))

      ;; Download the artifact archive.
      (ensure-directories-exist destination-directory)
      (cond
	((some (curry #'apply #'try-combination) combinations))

	(error?
	 (error "~@<Could not download artifact for system ~A.~@:>"
		name))

	(t
	 nil)))))


;;; Utility functions
;;

(defun %extract-archive (pathname
			 &key
			 (destination-directory
			  (truename (merge-pathnames
				     (make-pathname :name :unspecific
						    :type :unspecific)
				     pathname))))
  "TODO(jmoringe): document"
  (let ((output (make-string-output-stream))
	(error  (make-string-output-stream)))
    (unless (zerop
	     (sb-ext:process-exit-code
	      (sb-ext:run-program
	       "sh" (list "-c" (format nil "cd ~A && tar -xzf ~A"
				       destination-directory pathname))
	       :search t
	       :wait   t
	       :output output
	       :error  error)))
      (error "~@<Could not extract artifact archive ~S:~_~A~A~@:>"
	     pathname
	     (get-output-stream-string output)
	     (get-output-stream-string error)))))
