;;;; macros.lisp --- Macros used in the steps module.
;;;;
;;;; Copyright (C) 2014-2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.project.steps)

;;; Step definition

(defmacro define-step ((name
                        &key
                        (class      name)
                        (designator (make-keyword name)))
                       (&rest args)
                       &body body)
  (let+ (((&flet+ make-parameter ((name
                                   &optional
                                   (default `(missing-required-argument
                                              ,(make-keyword name)))))
            `(,name ,default)))
         ((&values body declarations documentation)
          (parse-body body :documentation t)))
    `(progn
       (defclass ,class ()
         ()
         ,@(when documentation
             `((:documentation ,documentation))))

       (service-provider:register-provider/class
        'step ,designator :class ',class)

       (defmethod execute
           ((step ,class) (context t)
            &key
            ,@(mapcar (compose #'make-parameter #'ensure-list) args))
         ,@declarations
         ,@body))))

;;; Sequence processing

(macrolet
    ((define-map (name parallel? progress?)
       `(defun ,name (sequence thunk combination
                      operation restart-report)
          ,@(unless progress? '((declare (ignore operation))))
          (,@(if progress?
                 '(with-sequence-progress (operation sequence))
                 '(progn))
           (let* ((thunk  (ensure-function thunk))
                  (action (ecase combination
                            (collect (lambda (item)
                                       (list (funcall thunk item #'progress))))
                            (append  (lambda (item)
                                       (funcall thunk item #'progress))))))
             (,(if parallel? 'lparallel:pmapcan 'mapcan)
              (lambda (item)
                (restart-case
                    (funcall action item)
                  (continue (&optional condition)
                    :report (lambda (stream)
                              (funcall restart-report stream item))
                    (declare (ignore condition)))))
              ,@(when parallel? '(:parts most-positive-fixnum))
              sequence))))))

  (define-map map-with-restart/sequential/progress nil t)
  (define-map map-with-restart/sequential          nil nil)

  (define-map map-with-restart/parallel/progress   t   t)
  (define-map map-with-restart/parallel            t   nil))

(defmacro with-sequence-processing ((designator item-var sequence-var
                                     &key
                                     (execution   :sequential)
                                     (combination 'collect)
                                     (progress    t))
                                    &body body)
  (let ((driver    (ecase execution
                     (:sequential (if progress
                                      'map-with-restart/sequential/progress
                                      'map-with-restart/sequential))
                     (:parallel   (if progress
                                      'map-with-restart/parallel/progress
                                      'map-with-restart/parallel))))
        (operation (if (eq progress t)
                       designator
                       progress))
        (report    (format nil "~~@<Skip ~A ~~S.~~@:>" designator)))
    (with-unique-names (progress-var)
      `(flet ((report (stream item)
                (format stream ,report item)))
         (,driver ,sequence-var
         (lambda (,item-var ,progress-var)
           ,@(unless progress `((declare (ignore ,progress-var))))
           (,@(if progress
                  `(flet ((progress (&rest args)
                            (declare (dynamic-extent args))
                            (apply ,progress-var args)))
                     (declare (dynamic-extent #'progress)))
                  '(progn))
             ,@body))
          ',combination ,operation #'report)))))

;;; Sequence step

(defmacro define-sequence-step ((name item-var sequence-parameter
                                 &rest meta-args &key
                                                 (execution   :sequential)
                                                 (combination 'collect)
                                                 (designator  (make-keyword name))
                                                 (progress    t))
                                (&rest args)
                                &body body)
  (let+ (((&values body declarations documentation)
          (parse-body body :documentation t)))
    `(define-step (,name :designator ,designator
                         ,@(remove-from-plist
                            meta-args
                            :execution :combination :designator :progress))
         (,@args ,sequence-parameter)
       ,@(when documentation `(,documentation))
       (with-sequence-processing (,designator ,item-var ,sequence-parameter
                                  :execution   ,execution
                                  :combination ,combination
                                  :progress    ,progress)
         ,@declarations
         ,@body))))
