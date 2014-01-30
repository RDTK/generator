;;;; run-nodes.lisp ---
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :jenkins.management)

;;; Connection Management

(defun direct-ssh-connection ()
  "TODO(jmoringe): document"
  (lambda (node spec)
    `(&host (ssh ,(host node)) ,spec)))

(defun via-ssh-connection (host function)
  "TODO(jmoringe): document"
  (lambda (node spec)
    `(&host (ssh ,host) ,(funcall function node spec))))

(defun and-sudo (password function)
  "TODO(jmoringe): document"
  (lambda (node spec)
    (funcall function
	     node `(&host (sudo ,password) ,spec))))

(declaim (special *node-connection-function*))

(defvar *node-connection-function* (direct-ssh-connection)
  "TODO(jmoringe): document")

(defun map-nodes (function &optional (nodes (all-nodes)))
  "TODO(jmoringe): document"
  (let* ((lparallel:*debug-tasks-p* t)
	 (special-bindings `((*base-url* . ,*base-url*)
			     (*username* . ,*username*)
			     (*password* . ,*password*)))
	 (thunk (lambda (id-or-node)
		  (progv (mapcar #'car special-bindings)
		      (mapcar #'cdr special-bindings)
		    (let ((node (etypecase id-or-node
				  (string (node id-or-node))
				  (node   id-or-node))))
		      (handler-bind
			  ((error #'(lambda (condition)
				      (error "~@<Error on node ~A:~_~A~@:>"
					     node condition))))
			(funcall function node)))))))
    (lparallel:task-handler-bind
	(#+no (error #'lparallel:invoke-transfer-error))
      (handler-bind
	  ((lparallel:task-killed-error #'abort))
	(lparallel:pmapcar thunk nodes)))))

(defmacro do-nodes ((node &optional (nodes '(all-nodes)))
		    &body body)
  (once-only (nodes)
    `(map-nodes (lambda (,node)
		  (declare (ignorable ,node))
		  ,@body)
		,nodes)))

(macrolet
    ((define-run-function (name &body body)
       (let ((function-name (symbolicate name '#:/nodes)))
	 `(defun ,function-name  (spec
				   &rest args
				   &key
				   (nodes                    (all-nodes))
				   (node-connection-function *node-connection-function*)
				   &allow-other-keys)
	     "TODO(jmoringe): document"
	    (let ((args (remove-from-plist args :nodes :node-connection-function)))
	      (flet ((make-spec (node)
		       (parse-host-spec
			(funcall node-connection-function node spec))))
		,@(or body
		      `((do-nodes (node nodes)
			  (apply #',name (make-spec node) args))))))))))

  (define-run-function run)
  (define-run-function run/s)
  (define-run-function run/ss)
  (define-run-function run/lines)

  (define-run-function run/shell
      (flet ((run (node)
	       (handler-case
		   (apply #'run/ss (make-spec node) args)
		 (error (condition)
		   (princ condition)))))
	(format t "~{~{~A:~&~:[| <no output>~;~:*~<| ~@;~A~:>~]~}~2&~}"
		(do-nodes (node nodes)
		  (list (id node) (list (run node))))))))
