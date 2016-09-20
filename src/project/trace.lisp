;;;; trace.lisp --- Variable lookup tracing.
;;;;
;;;; Copyright (C) 2012, 2013, 2014, 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.project)

;;; Logging support
;;;
;;; Copied from log4cl and hacked for this use case.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun expand-log-to-stream (env level stream-var body)
    "Returns a FORM that is used as an expansion of log-nnnnn macros"
    (let ((level (position level log4cl-impl::+log-level-symbols+
                           :test #'string-equal)))
      (declare (type fixnum level))
      (log4cl-impl::with-package-naming-configuration (*package*)
        (let* ((logger-form (log4cl-impl::resolve-logger-form *package* env '()))
               (logger-symbol (gensym "logger"))
               (log-stmt (gensym "log-stmt"))
               (const-logger (when (constantp logger-form)
                               (let ((logger (eval logger-form)))
                                 (when (typep logger 'log4cl::logger)
                                   logger))))
               (check-type (unless const-logger
                             `(or (typep ,logger-symbol 'logger)
                                  (error 'type-error :expected-type 'logger
                                         :datum ,logger-symbol))))
               (pkg-hint
                (let ((sym (intern (symbol-name log4cl-impl::package-ref-sym) *package*)))
                  (when sym `(symbol-package ',sym)))))
          `(let ((,logger-symbol ,logger-form))
             #+sbcl(declare (sb-ext:muffle-conditions sb-ext:compiler-note))
             (when (locally (declare (optimize (safety 0) (debug 0) (speed 3)))
                     ,@(when check-type (list check-type))
                     (log4cl-impl::is-enabled-for ,logger-symbol ,level))
               (flet ((,log-stmt (,stream-var)
                        (declare (type stream ,stream-var))
                        ,@body))
                 (declare (dynamic-extent #',log-stmt))
                 (locally (declare (optimize (safety 0) (debug 0) (speed 3)))
                   (log4cl-impl::log-with-logger ,logger-symbol ,level #',log-stmt ,pkg-hint))))
             (values)))))))

(defmacro logging-to-stream ((level stream-var) &body body &environment env)
  (expand-log-to-stream env level stream-var body))

;;; Actual tracing

(defvar *traced-variables* '())
(defvar *trace-node* nil)

(defstruct (trace-node (:constructor make-trace-node (name container &optional raw-cell)))
  (name      nil :read-only t)
  (container nil :read-only t)
  (raw-cell  nil :read-only t)
  (value     nil)
  (children  '()))

(defun print-trace-tree (stream tree)
  (utilities.print-tree:print-tree
   stream
   tree
   (utilities.print-tree:make-node-printer
    (lambda (stream depth node)
      (declare (ignore depth))
      (format stream "~A in ~A" (trace-node-name node) (trace-node-container node))
      t)
    (lambda (stream depth node)
      (declare (ignore depth))
      (let ((raw-cell (trace-node-raw-cell node))
            (value    (trace-node-value node)))
        (if (and raw-cell (equal value (cdr raw-cell)))
            (format stream "value: ~<~@;~S~:>" (list value))
            (format stream "raw value: ~:[«not defined»~;~:*~<~@;~S~:>~]~@:_~@:_~
                            eff value: ~<~@;~S~:>"
                    (when raw-cell (list (cdr raw-cell))) (list value)))))
    #'trace-node-children)))

(defun %call-with-augmented-trace (name container raw-cell thunk)
  (let+ (((&values node report?)
          (cond
            (*trace-node*
             (let ((node (make-trace-node name container raw-cell)))
               (appendf (trace-node-children *trace-node*) (list node))
               node))
            ((not (member name *traced-variables*))
             *trace-node*)
            (t
             (let ((node (make-trace-node name container raw-cell)))
               (values node t)))))
         (*trace-node* node))
    (unwind-protect
         (let ((values (multiple-value-list (funcall thunk))))
           (when node
             (setf (trace-node-value node) (first values)))
           (values-list values))
      (when report?
        (logging-to-stream (:warn stream)
          (print-trace-tree stream node))))))

(defun call-with-augmented-trace (name container raw-cell thunk)
  (let ((actual-container (gethash raw-cell *variable-locations*)))
    (cond
      ((and container (eq container actual-container))
       (%call-with-augmented-trace name container raw-cell thunk))
      ((and container actual-container)
       (%call-with-augmented-trace
        name container nil
        (lambda ()
          (%call-with-augmented-trace name actual-container raw-cell thunk))))
      (actual-container
       (%call-with-augmented-trace name actual-container raw-cell thunk))
      (t
       (%call-with-augmented-trace name container raw-cell thunk)))))

(defmacro with-augmented-trace ((name container &optional raw-cell)
                                &body body)
  `(call-with-augmented-trace
    ,name ,container ,raw-cell (lambda () ,@body)))
