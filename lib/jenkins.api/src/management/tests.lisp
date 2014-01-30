
;;; Tests
;;

;; (drakma:http-request "https://ci.cor-lab.de/computer/ubuntu_lucid_64bit/api/json"
;;		     )
;;
;; (json:decode-json-from-string
;;  (sb-ext:octets-to-string
;;   (drakma:http-request "https://ci.cor-lab.de/computer/api/json")))
;;
;; (json:decode-json-from-string
;;  (sb-ext:octets-to-string
;;   (drakma:http-request "https://ci.cor-lab.de/job/rsb-cl-trunk/api/json?depth=2")))
;;
;; (stp:serialize
;;  (cxml:parse
;;   (drakma:http-request "https://ci.cor-lab.de/job/rsb-cl-trunk/config.xml"
;;		       :basic-authorization (list "jmoringe" "JaVbY3p7cs88."))
;;   (stp:make-builder))
;;  (cxml:make-character-stream-sink *standard-output*))

(print (all-nodes))

(print (node "ubuntu_lucid_32bit"))

#+no (with-output-to-file (stream "/tmp/bli.xml" :if-exists :supersede)
       (let ((h (cxml:make-character-stream-sink stream)))
	 (stp:serialize (node-config "ubuntu_lucid_32bit") h)))


(let ((doc (node-config "ubuntu_lucid_32bit")))
  (xloc:with-locations (((:val env :type 'string/node)
			 "slave/nodeProperties/hudson.slaves.EnvironmentVariablesNodeProperty/envVars/tree-map/string"
			 :if-multiple-matches :all))
      doc
    (appendf env (list "SBCL_HOME" "/vol/cl/sbcl-1.57.0/lib/sbcl")))
  (stp:serialize doc (cxml:make-character-stream-sink *standard-output*)))

(print (all-jobs))

(print (job "rsb-cl-trunk"))



(iter:iter (iter:for job in (all-jobs))
	   (let ((config (job-config job)))

	     (with-output-to-file (stream (format nil "/tmp/~A-before.xml" job)
					  :if-exists :supersede)
	       (stp:serialize config (cxml:make-character-stream-sink stream :indentation 2)))

	     (ignore-some-conditions (cxml-location:xpath-creation-error)
	       (xloc:with-locations (((:val slaves :type 'string/node)
				      "/matrix-project/axes/hudson.matrix.LabelAxis[name/text()='label']/values/string"
				      :if-multiple-matches :all ))
		   config
		 (when slaves
		   (let ((clean (remove-if (lambda (name)
					     (member name '("ubuntu_natty_32bit"
							    "ubuntu_natty_64bit")
						     :test #'string=))
					   slaves)))
		     (unless (equal slaves clean)
		       (format t "~S~%" (list job slaves clean))
		       (setf slaves clean))))
		 (setf (job-config job) config)

		 (with-output-to-file (stream (format nil "/tmp/~A-after.xml" job)
					      :if-exists :supersede)
		   (stp:serialize config (cxml:make-character-stream-sink stream :indentation 2)))))))


#+no
(make-job "foo"
	  (dsl:job
	   (dsl:svn "https://foo")
	   (dsl:builders
	    (dsl:shell
	     "cp foo bar"
	     "rm bla"))))

(print (all-views))

(print (view "rsb-0.5"))

#+no
(make-view "bla" (cxml:with-xml-output (stp:make-builder)
		   (cxml:with-element "bla")))

#+no
(copy-job "rsb-manual-trunk" "foo")


#+no
(let* ((version    "0.7")
       (jobs       (all-jobs))
       (trunk-jobs (remove-if (complement (lambda (name) (ppcre:scan "^rsb(?:ag)?.*-trunk" name))) jobs)))
  (mapcar (lambda (name)
	    (let ((new-name (ppcre:regex-replace "trunk" name version)))
	      (unless (member new-name jobs :test #'string=)
		(format t "Copying job ~A -> ~A~%" name new-name)
		(copy-job new-name name))))
	  trunk-jobs))
