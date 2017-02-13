;;;; completion.lisp --- TODO.
;;;;
;;;; Copyright (C) 2016, 2017, 2018, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.language-server)

(defmethod complete ((context (eql :variable-name))
                     (prefix  string))
  (let+ (((&flet make-item (variable)
            (let ((name          (var:variable-info-name variable))
                  (type          (var:variable-info-type variable))
                  (documentation (var:variable-info-documentation variable)))
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
      (:items       . ,(or
                        (loop :for variable :in (var:all-variables)
                           :when (starts-with-subseq
                                  prefix (string-downcase (var:variable-info-name variable)))
                           :collect (make-item variable))
                        #() ; HACK
                        )))))

(defmethod complete ((context (eql :variable-value))
                     (prefix  string))
  (let+ ((variable (nth 3 (var:all-variables)))
         ((&flet make-item (label)
            `((:label . ,label)))))
    `((:is-complete . t)
      (:items       . ,(typecase (var:variable-info-type variable)
                         ((eql boolean)
                          (list (make-item "true") (make-item "false")))
                         ((eql string)
                          (list (make-item "\"\"")))
                         ((cons (eql jenkins.model::list-of) (cons (eql string) null))
                          (list (make-item "[ \"\" ]"))))))))
