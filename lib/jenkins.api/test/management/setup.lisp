;;;; setup.lisp ---
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.management.test)

#+no
(deploy-sbcl "azurit" "/tmp/sbcl-test/")

;; linux slaves
(dolist (slave '(("localhost" :port 4444)
		 #+no ("localhost" :port 4445)))
  (let+ (((host &rest args) slave))
   (bt:make-thread
    (lambda ()
      (apply #'install-sbcl host (format nil "/vol/cl/sbcl-~A" *sbcl-version*)
	     :host-lisp-program "/vol/cl/sbcl-1.0.54/bin/sbcl"
	     args)))))


;; macos slave
(install-sbcl "localhost" (format nil "/vol/cl/sbcl-~A" *sbcl-version*)
	      :port              4446
	      :host-lisp-program "/vol/cl/sbcl-1.0.54/bin/sbcl"
	      :environment       '("SBCL_HOME" "/vol/cl/sbcl-1.0.54/lib/sbcl")
	      :features          '(:sb-thread :sb-core-compression))

#+no
(let ((stream (open/ssh "localhost" "/tmp/bla.txt" :direction :output :port 22)))
  (format stream "bla~%")
  (close stream))

#+macabeo
(install-sbcl "macabeo" "/homes/abarch/opt/sbcl-1.0.54"
	      :user              "abarch"
	      :host-lisp-program "/homes/abarch/opt/sbcl/bin/sbcl"
	      :environment       '("SBCL_HOME" "/homes/abarch/opt/sbcl/lib/sbcl")
	      :features          '(:sb-core-compression))

#+no
(install-quicklisp "macabeo"
		   :user "abarch"
		   :lisp "/homes/abarch/opt/sbcl-1.0.54/bin/sbcl")
