;;;; restarts.lisp --- String-related utilities.
;;;;
;;;; Copyright (C) 2015, 2016, 2018, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:build-generator.util)

;;; Conditions

(defun some-cause (predicate condition)
  (labels ((rec (condition)
             (cond ((funcall predicate condition)
                    t)
                   ((when-let ((cause (more-conditions:cause condition)))
                      (rec cause)))
                   (t
                    nil))))
    (rec condition)))

;;; Continuing

(deftype continuable-error ()
  ;; SBCL establishes `continue' restarts when signaling the following
  ;; conditions. Using these restarts in an error policy causes an
  ;; infinite loop (and potentially hides a programming error). So
  ;; don't consider these errors continuable.
  `(not (or unbound-variable undefined-function)))

(defun find-continue-restart (condition)
  ;; Recent SBCL versions establish a `continue' restart in `open'
  ;; that actually just retries the operation. Using this restart in
  ;; an error policy causes an infinite loop. So ignore the
  ;; problematic restart when looking for `continue' restarts.
  #+sbcl (find-if (lambda (restart)
                    (and (eq (restart-name restart) 'continue)
                         (not (search "Retry opening"
                                      (princ-to-string restart)))))
                  (compute-restarts condition))
  #-sbcl (find-restart 'continue condition))

;;; Retrying

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
