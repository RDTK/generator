;;;; language-server.lisp --- TODO.
;;;;
;;;; Copyright (C) 2016, 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

;;;; https://github.com/Microsoft/language-server-protocol/

(cl:in-package #:jenkins.language-server)

(defun language-server (input output)
  (catch 'exit
    (with-output-to-file (*trace-output* "/tmp/trace" :if-exists :supersede)
      (loop :with context = (make-instance 'context)
         :with connection = (make-instance 'connection :input input :output output)
         :do (process-request connection context)))))

(defun process-request (connection context)
  (let+ (((&values id method arguments) (read-request connection))
         ((&values result condition)
          (ignore-errors (apply #'process-method context method arguments))))
    (when condition
      (format *trace-output* "Error: ~A~%" condition)
      (force-output *trace-output*))

    ; (print request *trace-output*)
    ; (print response *trace-output*)
    ; (force-output *trace-output*)

    (cond
      ((eq result :exit)
       (throw 'exit nil))
      ((not id))
      (t
       (write-response connection id result)))))

;;; TOOO move somewhere

(defmethod complete ((context (eql :variable-name))
                     (prefix  string))
  (let+ (((&flet make-item (variable)
            (let ((name          (jenkins.model.variables:variable-info-name variable))
                  (type          (jenkins.model.variables:variable-info-type variable))
                  (documentation (jenkins.model.variables:variable-info-documentation variable)))
              `((:insert-text   . ,(typecase type
                                     ((eql string)
                                      (format nil "~(~A~): \"\""
                                              name))
                                     ((cons (eql jenkins.model::list-of) (cons (eql string) null))
                                      (format nil "~(~A~): [ \"\" ]"
                                              name))
                                     (t
                                      (format nil "~(~A~): " name))))
                (:label         . ,(string-downcase name))
                (:detail        . ,(format nil "Type: ~A " type))
                (:documentation . ,documentation))))))
    `((:is-complete . t)
      (:items       . ,(loop :for variable :in (jenkins.model.variables:all-variables)
                          :when (starts-with-subseq
                                 prefix (string-downcase (jenkins.model.variables:variable-info-name variable)))
                          :collect (make-item variable))))))

(defmethod complete ((context (eql :variable-value))
                     (prefix  string))
  (let+ ((variable (nth 3 (jenkins.model.variables:all-variables)))
         ((&flet make-item (label)
            `((:label . ,label)))))
    `((:is-complete . t)
      (:items       . ,(typecase (jenkins.model.variables:variable-info-type variable)
                         ((eql boolean)
                          (list (make-item "true") (make-item "false")))
                         ((eql string)
                          (list (make-item "\"\"")))
                         ((cons (eql jenkins.model::list-of) (cons (eql string) null))
                          (list (make-item "[ \"\" ]"))))))))
