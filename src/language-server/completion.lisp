;;;; completion.lisp --- TODO.
;;;;
;;;; Copyright (C) 2016, 2017, 2018, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:build-generator.language-server)

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
                                     ((cons (eql model::list-of) (cons (eql string) null))
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
                         ((cons (eql model::list-of) (cons (eql string) null))
                          (list (make-item "[ \"\" ]"))))))))

;;;

(defclass structure-completion-contributor ()
  ())

(defmethod contrib:completion-contributions
    ((workspace   t)
     (document    project-document)
     (context     structure-context)
     (contributor structure-completion-contributor))
  (let+ ((path (path context))
         ((&flet make-item (section)
            (proto:make-completion-item (string-downcase section)))))
    (log:error path)
    (cond ((and (<= 1 (length path) 2) (ends-with :versions path))
           (map 'list #'make-item '(:name :variables)))
          ((length= 1 path)
           (map 'list #'make-item '(:templates :catalog :variables :versions))))))

(defmethod contrib:completion-contributions
    ((workspace   t)
     (document    distribution-document)
     (context     structure-context)
     (contributor structure-completion-contributor))
  (let+ ((path (path context))
         ((&flet make-item (section)
            (proto:make-completion-item (string-downcase section)))))
    (cond ((length= 1 path)
           (map 'list #'make-item '(:catalog :variables :versions)))
          ((and (length= 2 path) (ends-with :versions path))
           (map 'list #'make-item '(:name :versions :variables))))))

;;;

(defclass template-name-completion-contributor () ())

(defmethod contrib:completion-contributions
    ((workspace   t)
     (document    t)
     (context     template-name-context)
     (contributor template-name-completion-contributor))
  (let+ (((&flet make-item (template)
            (proto:make-completion-item (model:name template)))))
    (loop :for template :in (templates workspace)
          ;; when
          :collect (make-item template))))

;;;

(defclass variable-name-completion-contributor ()
  ())

(defmethod contrib:completion-contributions
    ((workspace   t)
     (document    t)
     (context     variable-name-context)
     (contributor variable-name-completion-contributor))
  (let+ (((&flet make-item (variable)
            (let* ((name          (var:variable-info-name variable))
                   (type          (var:variable-info-type variable))
                   (documentation (var:variable-info-documentation variable))
                   (title         (string-downcase name))
                   (new-text      (typecase type
                                    ((eql string)
                                     (format nil "~(~A~): \"\""
                                             name))
                                    ((cons (eql model::list-of) (cons (eql string) null))
                                     (format nil "~(~A~):~%- "
                                             name))
                                    (t
                                     (format nil "~(~A~): " name)))))
              (proto:make-completion-item
               title
               :kind          :variable
               :detail        (format nil "Type: ~A " type)
               :documentation documentation
               :new-text      new-text)))))
    (loop :for variable :in (var:all-variables)
             #+no :when #+no (starts-with-subseq
                              prefix (string-downcase (var:variable-info-name variable)))
          :collect (make-item variable))))

(defclass variable-value-completion-contributor ()
  ())

(defmethod possible-values ((type t))
  '())

(defmethod possible-values ((type (eql 'boolean)))
  (list "false" "true"))

(defmethod possible-values ((type cons))
  (possible-values-using-head type (first type)))

(defmethod possible-values-using-head ((type cons) (head t))
  '())

(defmethod possible-values-using-head ((type cons) (head (eql 'or)))
  (mappend #'possible-values (rest type)))

(defmethod possible-values-using-head ((type cons) (head (eql 'eql)))
  (list (format nil "~(~A~)" (second type))))

(defmethod contrib:completion-contributions
    ((workspace    t)
     (document     t)
     (context      variable-value-context)
     (contriubutor variable-value-completion-contributor))
  (let ((variable (variable-node context)))
    (map 'list (lambda (value)
                 (proto:make-completion-item value :kind :constant))
         (possible-values (var:variable-info-type variable)))))

(defclass document ()
  ((index :initarg :index
          :reader  index)))

#+no (let* ((source   (text.source-location:make-source "<string>"))
            (index    (text.source-location.lookup:make-index))
            (document (make-instance 'document :index index)))
       (values (language.yaml:load "variables:
  foo: bar"
                                   :builder (project::make-builder
                                             source index))
               index
               (lookup:lookup (text.source-location:make-location source 13 13) index)
               (contrib::context-contributions
                t
                document
                (text.source-location:make-location source 13 13)
                (make-instance 'variable-name-context-contributor))))
