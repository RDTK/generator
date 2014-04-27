;;;; conditions.lisp --- Conditions used in the commandline-interface module.
;;;;
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.project.commandline-interface)

;;; Error reporting

(defun report-error (stream condition &optional colon? at?)
  (declare (ignore at?))
  (typecase condition
    (deferred-phase-problem-condition
     (princ condition stream))
    (unfulfilled-dependency-condition
     (format stream "~@<~A: ~A~@:>"
             (instantiation-condition-specification condition)
             (root-cause condition)))
    (t
     (let+ (((&flet output ()
               (format stream "~@<~A:~
                               ~@:_~2@T~<~A~:>~
                               ~:>"
                       (type-of condition) (list condition)))))
       (if colon?
           (ignore-errors
            (alexandria:unwind-protect-case ()
                (output)
              (:abort
               (format stream "<error printing condition>~%"))))
           (output))))))

(defun print-unfulfilled-dependency (stream dependency &optional colon? at?)
  (declare (ignore colon? at?))
  (let+ (((object . conditions) dependency))
    (format stream "~@<~A (~D missing dependenc~@:P):~
                    ~@:_~2@T~@<~{~A~^~@:_~}~:>~
                    ~:>"
            object (length conditions) conditions)))

(defun print-unfulfilled-dependencies (stream conditions &optional colon? at?)
  (declare (ignore colon? at?))
  (let+ ((table (make-hash-table :test #'eq)))
    (dolist (condition conditions)
      (let ((object (instantiation-condition-specification condition)))
        (appendf (gethash object table '()) (list (root-cause condition)))))
    (format stream "~@<~{~
                      ~/jenkins.project.commandline-interface::print-unfulfilled-dependency/~
                      ~^~@:_~@:_~
                    ~}~:>"
            (hash-table-alist table))))

;;; Unfulfilled dependencies

(defun unfulfilled-dependency-condition? (condition)
  (when (typep condition 'instantiation-error)
    (let ((root-cause (root-cause condition)))
      (when (typep root-cause 'simple-error)
        (search "No provider" (simple-condition-format-control root-cause)))))) ; TODO do this properly

(deftype unfulfilled-dependency-condition ()
  '(satisfies unfulfilled-dependency-condition?))

;;; Phase conditions

(define-condition phase-condition (condition)
  ((phase :initarg :phase
          :type    :keyword
          :reader  phase-condition-phase
          :documentation
          "Stores the execution phase during which the condition was
           signaled."))
  (:default-initargs
   :phase (missing-required-initarg 'phase-condition :phase)))

(define-condition phase-error (error
                               phase-condition)
  ())

(define-condition simple-phase-error (simple-error
                                      phase-error)
  ())

(define-condition deferred-phase-problem-condition (phase-condition)
  ((conditions :initarg :conditions
               :type     list
               :reader   phase-condition-conditions
               :initform '()
               :documentation
               "Stores the conditions that were deferred during the
                execution of a particular phase.")))

(define-condition deferred-phase-error (phase-error
                                        deferred-phase-problem-condition)
  ()
  (:default-initargs
   :problems (missing-required-initarg 'deferred-phase-error :conditions))
  (:report
   (lambda (condition stream)
     (let+ (((&structure-r/o phase-condition- phase conditions) condition))
       (format stream "~@<~D problem~:P during ~A phase:~@:_~@:_~
                       ~2@T~<~{~
                         ~:/jenkins.project.commandline-interface::report-error/~
                         ~^~@:_~@:_~
                         ~}~
                         ~/jenkins.project.commandline-interface::print-unfulfilled-dependencies/~
                       ~:>~@:>"
               (length conditions) phase
               (list (remove-if #'unfulfilled-dependency-condition? conditions)
                     (remove-if-not #'unfulfilled-dependency-condition? conditions)))))))
