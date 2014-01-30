;;;; util.lisp ---
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.scripting)

(defun %diff-configs (name-a config-a name-b config-b
                      &optional
                      (stream *standard-output*))
  (let+ (((&flet write-config (name dom)
            (let ((filename (format nil "/tmp/~A.xml" name)))
              (write-string-into-file
               (stp:serialize dom (cxml:make-string-sink))
               filename
               :if-does-not-exist :create
               :if-exists         :supersede)
              filename)))
         (filename-a (write-config name-a config-a))
         (filename-b (write-config name-b config-b)))
    (unwind-protect
         (sb-ext:run-program "diff" (list "-u" filename-a filename-b)
                             :search t
                             :wait   t
                             :output stream)
      (ignore-errors (delete-file filename-a))
      (ignore-errors (delete-file filename-b)))
    (values)))

(defmethod diff-configs ((a string) (b string))
  (diff-configs (job a) (job b)))

(defmethod diff-configs ((a job) (b job))
  (%diff-configs (id a) (job-config (id a))
                 (id b) (job-config (id b))))

(defmethod diff-configs ((a node) (b node))
  (%diff-configs (id a) (node-config (id a))
                 (id b) (node-config (id b))))
