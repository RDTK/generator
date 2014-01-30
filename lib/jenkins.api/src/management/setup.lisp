;;;; setup.lisp ---
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.management)


;;; Install functions
;;

#+later (with-condition-translation
    (((error install-error)
      :slave     ,host
      :component ,(string component)))
  (loop (restart-case
	    (progn
	      ,@(butlast body)
	      (return-from ,name ,(lastcar body)))
	  (retry ())
	  (continue ()
	    (return-from ,name nil)))))

(defmacro define-install-function (component (host &rest args) &body body)
  "TODO(jmoringe): document"
  (let ((name (symbolicate '#:install- component)))
    `(defun ,name ,(cons host args)
       ,@body)))

(defmacro with-steps (op &body spec)
  "TODO(jmoringe): document"
  (iter (for step in spec)
	(for i :from 0)
	(collect `'(echo ,i) :into steps)
	(collect step :into steps)
	(counting 1 :into num-steps)
	(finally (return `(values (list ',op ,@steps) ,num-steps)))))

(define-install-function packages (packages
				   &key
				   update?)
  `(progn
     ,@(when update?
	 '((apt-get update)))
     (apt-get install -y ,@(ensure-list packages))))

(define-install-function sbcl (destination-directory
			       &key
			       (base-url          "http://prdownloads.sourceforge.net/sbcl/")
			       (version           *sbcl-version*)
			       (features          '(:sb-thread :sb-core-compression (not :sb-doc)))
			       (host-lisp-program "sbcl")
			       (host-lisp-options '("--no-userinit"))
			       (host-lisp         (format nil "~A ~{~A~^ ~}"
							  host-lisp-program
							  host-lisp-options)))
  "TODO(jmoringe): document"
  (let+ ((name    (format nil "sbcl-~A" version))
	 (archive (format nil "~A-source.tar.bz2" name))
	 (url     (format nil "~A~A" base-url archive))
	 ((&flet make-feature (feature)
	    (etypecase feature
	      (keyword
	       `(pushnew ,feature features))
	      ((cons (eql not) (cons keyword null))
	       `(setf features (delete ,(second feature) features)))))))
    (with-steps and
      ;; Download and extract
      '(cd "/tmp")
      '(rm -rf "sbcl")
      #+no `(rm -rf ,name)
      #+no `(curl :silent "-L" -o ,archive ,url)
      #+no `(tar -xjf ,archive)
      #+no `(rm ,archive)
      #+no `(cd ,name)

      '(git clone "http://git.code.sf.net/p/sbcl/sbcl")
      '(cd "sbcl")
      `(git checkout ,version)

      ;; Configure
      `(echo ,(let ((*package* #.*package*))
		(format nil "~S"
			`(lambda (features)
			   ,@(mapcar #'make-feature features)
			   features)))
	     (> "customize-target-features.lisp"))
      `(bash "make.sh"
	     ,(format nil "--xc-host=~A" host-lisp)
	     ,(format nil "--prefix=~A" destination-directory)
	     (>& 2 1))

      ;; Install
      `(bash -c ,(format nil "SBCL_HOME=~A/lib/sbcl INSTALL_ROOT=~:*~A bash install.sh"
			 destination-directory)))))


(define-install-function quicklisp (&key
				    (destination-directory "~/.local/share/common-lisp/quicklisp/")
				    (lisp                  "sbcl")
				    (quicklisp-url         "http://beta.quicklisp.org/quicklisp.lisp"))
  "TODO(jmoringe): document"
  `(progn
     (cd "/tmp")
     (curl -O ,quicklisp-url)
     (,lisp :load "/tmp/quicklisp.lisp"
	    :eval "(quicklisp-quickstart:install :path \"${destination-directory}\")"
	    :eval "(with-open-file (stream \"~/.sbclrc\"
                                           :direction         :output
                                           :if-does-not-exist :create
                                           :if-exists         :append)
                                      (ql-impl-util::write-init-forms stream))")))

(define-install-function git (repository-url
			      &key
			      (destination-directory "~/.local/share/common-lisp/quicklisp/local-projects/")
			      (git                   "git"))
  "TODO(jmoringe): document"
  `(progn
     (mkdir -p ,destination-directory)
     (cd ,destination-directory)
     (,git clone ,repository-url)))

(define-install-function local (directory
				&key
				(destination-directory "~/.local/share/common-lisp/quicklisp/local-projects/"))
  "TODO(jmoringe): document"
  (let ((archive (format nil "/tmp/~A.tar.gz" (gensym "temp"))))
    `(:local
      (tar :exclude-vcs
	   :transform (format nil "s#^~A##" (make-pathname :directory (cons :relative (butlast (rest (pathname-directory (parse-namestring directory)))))))
	   -czf ,archive
	   ,directory)
     :remote
     (mkdir -p ,destination-directory)
     :local
     (scp ,archive ,destination-directory)
     :remote
     (progn
	(cd ,destination-directory)
	(tar -xzf ,archive)
	(rm ,archive)))))

(defun setup-slave (hostname prefix
		    &key
		    (features          '(:sb-thread :sb-core-compression (not :sb-doc)))
		    (host-lisp-program "sbcl")

		    user
		    port
		    (environment       nil    environment-supplied?)

		    clean?
		    (steps             '(:packages :sbcl :quicklisp :local-projects :permissions))
		    (local-projects    '((:git   "https://github.com/scymtym/more-conditions.git")
					 (:git   "https://github.com/scymtym/print-items.git")
					 (:git   "https://github.com/scymtym/protocl.git")
					 (:git   "https://github.com/scymtym/esrap.git")
					 (:local "/homes/jmoringe/code/trivial-coverage/")
					 (:local "/homes/jmoringe/code/cl/alexandria/")
					 (:local "/homes/jmoringe/code/cl/iterate-1.4.3/")
					 (:local "/homes/jmoringe/code/cl/lift/")
					 (:local "/homes/jmoringe/code/cl/clon/")
					 (:local "/homes/jmoringe/.local/share/common-lisp/quicklisp/dists/quicklisp/software/asdf-system-connections-20101006-darcs/"))))
  "TODO(jmoringe): document"
  (let ((lisp-directory  (format nil "~A/sbcl-~A/"
				 prefix *sbcl-version*))
	(lisp-executable (format nil "~A/sbcl-~A/bin/sbcl"
				 prefix *sbcl-version*))
	(common          (append
			  (list :user user
				:port port)
			  (when environment-supplied?
			    (list :environment environment)))))
    ;; Home directory and ssh setup
    (when (intersection '(:home :ssh) steps)
      (apply #'create-home/ssh hostname user common))
    (when (find :ssh steps)
      (apply #'setup-key/ssh common))

    ;; Install required programs
    (when (find :packages steps)
      (apply #'install-packages hostname (list "curl" "git-core" "tar" host-lisp-program)
	     common))

    ;; [Clean], create and setup prefix
    (when clean?
      (apply #'run-shell/root/ssh hostname (format nil "rm -rf ~S" prefix)
	     common)
      (apply #'run-shell/root/ssh hostname (format nil "mkdir -p ~S" prefix)
	     common)
      (apply #'run-shell/root/ssh hostname (format nil "chown -R jenkins:ciadm ~S" prefix)
	     common)
      (apply #'run-shell/root/ssh hostname (format nil "chmod -R g+rwxs ~S" prefix)
	     common))

    ;; Step 1 install SBCL on the slave.
    (when (find :sbcl steps)
      (apply #'install-sbcl hostname lisp-directory
	     :host-lisp-program host-lisp-program
	     :features          features
	     common))

    ;; Step 2 install quicklisp on the slave.
    (when (find :quicklisp steps)
      (apply #'install-quicklisp hostname
	     :destination-directory (merge-pathnames "share/common-lisp/quicklisp/" prefix)
	     :lisp lisp-executable
	     common))

    (when (find :local-projects steps)
      (lparallel:pmapcar
       (lambda+ ((method location))
	 (apply (ecase method
		  (:git   #'install-git)
		  (:local #'install-local))
		hostname location
		:destination-directory (merge-pathnames "share/common-lisp/quicklisp/local-projects/" prefix)
		common))
       local-projects))

    (when (find :permissions steps)
      (apply #'run-shell/root/ssh hostname (format nil "chown -R jenkins:ciadm ~S" prefix)
	     common)
      (apply #'run-shell/root/ssh hostname (format nil "find ~S -type d -exec chmod g+rwxs {} \\;" prefix)
	     common)
      (apply #'run-shell/root/ssh hostname (format nil "find ~S -type f -exec chmod g+rw {} \\;" prefix)
	     common))))

#+no (defun setup-slaves (specs prefix
		     &rest args
		     &key
		     port
		     &allow-other-keys)
  "TODO(jmoringe): document"
  (lparallel:pmapcar
   (lambda+ ((hostname &key ((:port port1) port) ((:prefix prefix1) prefix)))
     (apply #'setup-slave hostname prefix1 :port port1
	    (remove-from-plist args :port)))
   (mapcar #'ensure-list specs)))


#+no (defun setup-slave/all-nodes (prefix &rest args
				     &key
				     (nodes (all-nodes))
				     &allow-other-keys)
  "TODO(jmoringe): document"
  (do-nodes ((node host port) nodes)
    (multiple-value-list
     (apply #'setup-slave host prefix
	    :port port
	    (remove-from-plist args :nodes)))))
