;;;; restarts.lisp --- String-related utilities.
;;;;
;;;; Copyright (C) 2015, 2016, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.util)

(defun call-with-retry-restart (thunk report)
  (tagbody
   :start
     (restart-case
         (return-from call-with-retry-restart
           (funcall thunk))
       (retry ()
         :report (lambda (stream) (funcall report stream))
         (go :start)))))

(defmacro with-retry-restart ((format-control &rest format-arguments) &body body)
  `(call-with-retry-restart
    (lambda () ,@body)
    (lambda (stream)
      (format stream ,format-control ,@format-arguments))))

(defun call-with-retries (thunk condition-type limit)
  (let ((retry-count 0))
    (handler-bind
        ((condition (lambda (condition)
                      (cond
                        ((not (typep condition condition-type)))
                        ((>= retry-count limit)
                         (error "~@<Giving up after ~D ~
                                 retr~:@P. Original error: ~A~@:>"
                                retry-count condition))
                        (t
                         (when-let ((restart (find-restart 'retry condition)))
                           (incf retry-count)
                           (log:warn "~@<Invoking retry restart ~A, ~
                                      attempt ~D of ~D.~@:>"
                                     restart retry-count limit)
                           (invoke-restart restart)))))))
      (funcall thunk))))

(defmacro with-retries ((condition-type &key limit) &body body)
  `(call-with-retries (lambda () ,@body) ',condition-type ,limit))
