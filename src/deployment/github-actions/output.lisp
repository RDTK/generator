;;;; output.lisp --- Rendering workflow nodes as YAML.
;;;;
;;;; Copyright (C) 2020 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:build-generator.deployment.github-actions)

;;; Utilities

(defun call-with-key-and-indented-value (thunk stream key)
  (princ key stream)
  (write-char #\: stream)
  (pprint-newline :mandatory stream)
  (pprint-logical-block (stream nil :per-line-prefix "  ")
    (funcall thunk stream)))

(defmacro with-key-and-indented-value ((stream key) &body body)
  `(call-with-key-and-indented-value
    (lambda (,stream) ,@body) ,stream ,key))

;;; Workflow and job

(defmethod output ((node workflow) (target stream))
  (format target "name: ~A~@:_~
                  ~@:_~
                  on: [push]~@:_~
                  ~@:_"
          (name node))
  (with-key-and-indented-value (target "jobs")
    (map nil (lambda (job)
               (output job target)
               (pprint-newline :mandatory target))
         (jobs node))))

(defmethod output ((node job) (target stream))
  (with-key-and-indented-value (target (model:name node))
    (format target "runs-on: ~A~@:_" (runs-on node))
    (when-let ((needs (needs node)))
      (format target "needs: [~{~A~^,~}]~@:_"
              (map 'list #'model:name needs)))
    (with-key-and-indented-value (target "steps")
      (map nil (lambda (step)
                 (write-string "- " target)
                 (pprint-logical-block (target (list step))
                   (output step target))
                 (pprint-newline :mandatory target))
           (steps node)))))

;;; Steps

(defmethod output ((node step) (target stream))
  (format target "name: ~A~@:_" (model:name node)))

(defmethod output ((node action-step-mixin) (target stream))
  (call-next-method)
  (format target "uses: ~A~@:_" (uses node)))

(defmethod output ((node action-step) (target stream))
  (call-next-method)
  (when-let ((parameters (parameters node)))
    (format target "with:~@:_~
                    ~2@T~@<~{~(~A~): ~A~^~@:_~}~:>"
            parameters)))

(defmethod output ((node command-step) (target stream))
  (call-next-method)
  (let ((lines (split-sequence:split-sequence #\Newline (deploy:command node))))
    ;; TODO WORKSPACE is a hack
    (format target "run: |~@:_~
                    ~2@Texport WORKSPACE=$(pwd)~@:_~
                    ~2@T~@<~{~A~^~:@_~}~:>~@:_"
            lines)))

(defmethod output ((node checkout-step) (target stream))
  (call-next-method)
  (with-key-and-indented-value (target "with")
    (format target "~@[repository: ~A~@:_~]~
                    ~@[ref: ~A~@:_~]~
                    ~@[fetch-depth: ~A~@:_~]"
            (repository node) (ref node) (depth node))))
