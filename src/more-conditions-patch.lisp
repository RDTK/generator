(cl:in-package #:more-conditions)

(defvar *without-progress* nil)

(defmacro without-progress (&body body)
  `(let ((*without-progress* t))
     ,@body))

(defun %progress (&optional operation progress
                            format-control-or-condition-class
                  &rest format-arguments-or-initargs)
  (declare (type progress-designator progress))
  (when *without-progress*
    (return-from %progress))
  (typecase format-control-or-condition-class
    ((or string function) ; assume formatter when function
     (signal 'simple-progress-condition
             :operation        operation
             :progress         progress
             :format-control   format-control-or-condition-class
             :format-arguments format-arguments-or-initargs))
    (t
     (apply #'signal (or format-control-or-condition-class
                         'progress-condition)
            :operation operation
            :progress  progress
            format-arguments-or-initargs))))
