;;;; resources.lisp ---
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.scripting)

(defun counter (&key (start 4000) (step '(("spread_port" . 2)
                                          ("socket_port" . 60)
                                          (t             . 1))))
  "Return a function, which when called with one argument - a resource
   name - returns a port number for that resource."
  (let ((state start))
    (lambda (name)
      (let ((step (or (cdr (assoc name step :test #'string=))
                      (cdr (assoc t step :test #'eq))
                      1)))
        (prog1
            state
          (incf state step))))))

(defun assign-unique-ports (&key
                            (jobs      (all-jobs))
                            (generator (counter)))
  (dolist (job jobs)
    (when (and (environment job)
               (some (compose (curry #'ppcre:scan "port") #'string-downcase)
                     (environment job)))
      (setf (environment job)
            (iter (for (key value) on (environment job) :by #'cddr)
                  (collect key)
                  (collect
                      (if (ppcre:scan "port" (string-downcase key))
                          (princ-to-string (funcall generator (string-downcase key)))
                          value))))
      (format t "~A~%~2@T~S~%" (id job) (environment job))
      (commit! job))))
