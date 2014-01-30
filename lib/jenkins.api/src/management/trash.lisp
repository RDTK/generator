(defvar *node-connections* (make-hash-table :test #'equal)
  "TODO(jmoringe): document")

(defvar *connections-lock* (bt:make-lock "connections lock")
  "TODO(jmoringe): document")

(defclass node-connection ()
  ((node    :initarg  :node
	    :type     string
	    :reader   connection-node
	    :accessor %node
	    :documentation
	    "")
   (host    :type     string
	    :reader   host
	    :accessor %host
	    :documentation
	    "")
   (port    :type     (unsigned-byte 16)
	    :reader   port
	    :accessor %port
	    :documentation
	    "")
   (process :reader   process
	    :accessor %process
	    :documentation
	    ""))
  (:default-initargs
   :node (missing-required-initarg 'node-connection :node))
  (:documentation
   "TODO(jmoringe): document"))

(defmethod shared-initialize :after ((instance   node-connection)
                                     (slot-names t)
                                     &key
				     node)
  (let+ (((&accessors (host %host) (port %port) (process %process)) instance)
	 (node-host   (xloc:val (xloc:loc (node-config node)
					  "/slave/launcher/host/text()")))
	 (random-port (+ 1025 (random (- 65535 1025))))
	 (spec (format nil "-L~D:~A:22" random-port node-host)))
    (format t "setting up tunnel ~A:~D -> ~A:~D~%"
	    "localhost" random-port node-host 22)
    (setf host    "localhost"
	  port    random-port
	  process (sb-ext:run-program "ssh" (list spec "corci" "sleep" "100000")
				      :search t
				      :wait   nil
				      :output *standard-output*
				      :error  *error-output*)))
  (sleep 1)
  instance)

(defmethod usable? ((connection node-connection))
  "TODO(jmoringe): document"
  (sb-ext:process-alive-p (process connection)))

(defmethod print-object ((object node-connection) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A:~D -> ~A"
	    (host object) (port object) (connection-node object))))

(defun ensure-connection (node)
  "TODO(jmoringe): document"
  (bt:with-lock-held (*connections-lock*)
    (or (when-let ((connection (gethash node *node-connections*)))
	  (when (usable? connection)
	    connection))
	(setf (gethash node *node-connections*)
	      (make-instance 'node-connection
			     :node node)))))

(defun run-program/all-nodes (program args
			      &rest run-program-args
			      &key
			      (nodes (all-nodes))
			      &allow-other-keys)
  "TODO(jmoringe): document"
  (do-nodes ((node host port) nodes)
    (multiple-value-list
     (apply #'run-program/ssh host program args
	    :port port
	    (remove-from-plist run-program-args
			       :nodes)))))

(macrolet
    ((define-shell/all-nodes (name)
       (let ((name     (format-symbol *package* "~A/ALL-NODES" name))
	     (name/ssh (format-symbol *package* "~A/SSH" name)))
	 `(defun ,name (command
			&rest run-program-args
			&key
			(nodes (all-nodes))
			&allow-other-keys)
	    "TODO(jmoringe): document"
	    (format t "~{~{~A:~&~:[| <no output>~;~:*~<| ~@;~A~:>~]~}~2&~}"
		    (map-nodes (lambda (node host port)
				 (let ((result (apply #',name/ssh host command :port port
						      (remove-from-plist run-program-args :nodes))))
				   (list node (when result (list result)))))
			       nodes))))))

  (define-shell/all-nodes run-shell)
  (define-shell/all-nodes run-shell/root))

(defun run-shell/nodes (command
			&rest run-program-args
			&key
			(nodes (all-nodes))
			&allow-other-keys)
  (format t "~{~{~A:~&~:[| <no output>~;~:*~<| ~@;~A~:>~]~}~2&~}"
	  (map-nodes (lambda (node host port)
		       (let ((result (inferior-shell:run/ss
				      command
				      :host (list host :port port)
				      :show t
				      #+where-should-these-go (remove-from-plist run-program-args :nodes))))
			 (list node (when result (list result)))))
		     nodes)))


;;; Basic setup functions
;;

(defun/ssh create-home (host for-user
			     &rest args
			     &key
			     (group "ciadm")
			     &allow-other-keys)
  (apply #'run-shell/root/ssh host
	 (format nil "mkdir -p /home/~A && chown -R ~:*~A:~A /home/~2:*~A/"
		 for-user group)
	 args))

(defun/ssh setup-key (host &rest args &key &allow-other-keys)
  (let ((key (read-file-into-string "~/.ssh/id_rsa.pub")))
    (apply #'run-shell/ssh host
	   (format nil "mkdir -p \"${HOME}/.ssh\" && echo \"~A\" >> \"${HOME}/.ssh/authorized_keys\""
		   key)
	   args)))

(define-install-function packages (host packages
					&key
					update?
					environment
					user
					port)
  (when update?
    (run-shell-remotely/root "apt-get update"))
  (run-shell-remotely/root (format nil "apt-get install -y ~{~A~^ ~}" packages)))
