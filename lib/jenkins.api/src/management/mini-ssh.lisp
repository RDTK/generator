;;; mini-ssh.lisp ---
;;
;; Copyright (C) 2011, 2012 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This Program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This Program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses>.

(cl:in-package :jenkins.management)

(defvar *ssh-port* nil
  "TODO(jmoringe): document")

(defvar *ssh-user* nil
  "TODO(jmoringe): document")

(defvar *sudo-password* nil
  "TODO(jmoringe): document")

(defvar *sudo-password-lock* (bt:make-lock "Sudo password lock")
  "TODO(jmoringe): document")

(defun invoke-with-exit-code-translation (thunk
					  &key
					  streams
					  program
					  args)
  "TODO(jmoringe): document"
  (let* ((process (apply thunk streams))
	 (code    (sb-ext:process-exit-code process)))
    (unless (zerop code)
      (error "~@<Program~@[ ~S~]~@[ with arguments ~{~S~^, ~}~] failed ~
to execute with code ~D: ~{~A~}~@:>"
	     program args code
	     (mapcar #'get-output-stream-string streams)))
    (values process streams)))

(defmacro with-exit-code-translation ((&key
				       streams
				       program
				       args)
				      &body body)
  "TODO(jmoringe): document"
  (let+ (((&flet make-stream (var)
	    `(,var (make-string-output-stream)))))
   `(let (,@(mapcar #'make-stream (ensure-list streams)))
      (invoke-with-exit-code-translation
       (lambda (,@streams) ,@body)
       :streams (list ,@streams)
       :program ,program
       :args    ,args))))

(defun run-program/exit-code-translation (program args
					  &rest run-program-args
					  &key &allow-other-keys)
  "TODO(jmoringe): document"
  (with-exit-code-translation (:streams (output error)
			       :program program
			       :args    args)
    (apply #'sb-ext:run-program program args
	   :output output
	   :error  error
	   :wait   t
	   run-program-args)))

(defun run-shell/exit-code-translation (program args
					&rest run-program-args
					&key &allow-other-keys)
  "TODO(jmoringe): document"
  (let+ (((&flet extract-output (stream)
	    (string-right-trim
	     '(#\Newline) (get-output-stream-string stream))))
	 ((&values nil (output error))
	  (apply #'run-program/exit-code-translation
		 program args
		 :search t
		 :wait   t
		 run-program-args)))
    (concatenate 'string (extract-output output) (extract-output error))))

(defmacro defun/ssh (name (&rest args) &body body)
  "TODO(jmoringe): document"
  (let+ (((&values required optional rest keywords allow-other-keys?)
	  (parse-ordinary-lambda-list args :normalize t))
	 ((&values forms declarations doc)
	  (parse-body body))
	 (name (symbolicate name "/SSH")))
    `(defun ,name (,@required
		   ,@(when optional
		       `(&optional ,@optional))
		   ,@(when rest
		       `(&rest ,rest))
		   &key
		   ,@(mapcar (curry #'remove nil) keywords)
		   (port         *ssh-port*)
		   (user         *ssh-user*)
		   x-forwarding?
		   ,@(when allow-other-keys?
		       '(&allow-other-keys)))
       ,@(when doc `(,doc))
       ,@declarations
       (declare (ignorable user port x-forwarding?))
       ,@forms)))

(defun/ssh open (host filename
		 &key
		 (direction :input)
		 if-exists
		 if-does-not-exist)
  "TODO(jmoringe): document"
  (let* ((target  (format nil "~@[~A@~]~A" user host))
	 (process (ecase direction
		    (:input  (sb-ext:run-program
			      "ssh" (append
				     (when port
				       (list "-p" (princ-to-string port)))
				     (list target
					   (format nil "cat \"~A\"" filename)))
			      :search t
			      :wait   nil))
		    (:output (sb-ext:run-program
			      "ssh" (append
				     (when port
				       (list "-p" (princ-to-string port)))
				     (list target
					   (format nil "cat - > \"~A\"" filename)))
			      :search t
			      :wait   nil
			      :input  :stream)))))
    (ecase direction
      (:input  (sb-ext:process-output process))
      (:output (sb-ext:process-input  process)))))

(defun/ssh run-program (host program args
			&rest run-program-args
			&key &allow-other-keys)
  "TODO(jmoringe): document"
  (let ((target (format nil "~@[~A@~]~A" user host)))
    (with-exit-code-translation (:streams (output error)
				 :program program
				 :args    args)
      (apply #'sb-ext:run-program "ssh"
	     (append (when port
		       (list "-p" (princ-to-string port)))
		     (when x-forwarding?
		       (list "-X"))
		     (list target (format nil "~A ~{'~A'~^ ~}"
					  program args)))
	     :search t
	     :output output
	     :error  error :pty nil
	     (remove-from-plist run-program-args
				:user :port :x-forwarding?)))))

(defun/ssh run-shell (host command
		      &rest args
		      &key &allow-other-keys)
  "TODO(jmoringe): document"
  (let+ (((&values nil (output error))
	  (apply #'run-program/ssh host "sh" (list "-c" command)
		 :wait t
		 args))
	 ((&flet extract (stream)
	    (string-right-trim
	     '(#\Newline) (get-output-stream-string stream)))))
    (let ((output/string (extract output))
	  (error/string  (extract error)))
      (unless (and (emptyp output/string) (emptyp error/string))
	(concatenate 'string output/string error/string)))))

(defun sudo-password ()
  "TODO(jmoringe): document"
  (bt:with-lock-held (*sudo-password-lock*)
   (or *sudo-password*
       (progn
	 (format *query-io* "Sudo password: ")
	 (finish-output *query-io*)
	 (setf *sudo-password* (read-line *query-io*))))))

(defun/ssh run-shell/root (host command
			   &rest args
			   &key
			   (sudo-password (sudo-password))
			   &allow-other-keys)
  "Execute the shell command COMMAND on HOST as root user."
  (let ((command (format nil "echo \"#!/bin/sh
   echo ~A\" ~
   > /tmp/passwd.sh ~
&& chmod 700 /tmp/passwd.sh ~
&& SUDO_ASKPASS=/tmp/passwd.sh sudo -A ~A"
			   sudo-password command)))
    (apply #'run-shell/ssh host command
	   (remove-from-plist args :sudo-password))))

(defun/ssh copy-file (file host destination)
  "TODO(jmoringe): document"
  (let* ((source  (truename file))
	 (target  (format nil "~@[~A@~]~A:~A" user host destination)))
    (run-program/exit-code-translation
     "scp"
     (append (when port
	       (list "-P" (princ-to-string port)))
	     (list (namestring source) target))
     :search t)))

(defun/ssh copy-directory (directory host destination)
  "TODO(jmoringe): document"
  (let* ((source  (truename directory))
	 (target  (format nil "~@[~A@~]~A:~A" user host destination)))
    (run-program/exit-code-translation
     "scp"
     (append (when port
	       (list "-P" (princ-to-string port)))
	     (list "-r" (namestring source) target))
     :search t)))
