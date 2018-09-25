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

(defmethod contrib:completion-contributions
    ((workspace   t)
     (document    template-document)
     (context     structure-context)
     (contributor structure-completion-contributor))
  (let+ ((path (path context))
         ((&flet make-item (section)
            (proto:make-completion-item (string-downcase section)))))
    (cond ((length= 1 path)
           (map 'list #'make-item '(:inherit :variables :aspects :jobs)))
          ((and (length= 2 path) (ends-with :aspects path))
           (map 'list #'make-item '(:name :aspect :conditions :variables)))
          ((and (length= 2 path) (ends-with :jobs path))
           (map 'list #'make-item '(:name :conditions :variables))))))

;;;

(defclass template-name-completion-contributor () ())

(defmethod contrib:completion-contributions
    ((workspace   t)
     (document    t)
     (context     template-name-context)
     (contributor template-name-completion-contributor))
  (let+ ((templates (templates workspace))
         ((&flet make-item (template)
            (proto:make-completion-item (jenkins.model:name template)))))
    (when (lparallel:fulfilledp templates)
      (loop :for template :in (lparallel:force templates)
            ;; when
            :collect (make-item template)))))

;;;

(defclass variable-name-completion-contributor ()
  ())

(defmethod contrib:completion-contributions
    ((workspace   t)
     (document    t)
     (context     variable-name-context)
     (contributor variable-name-completion-contributor))
  (let+ (((&flet make-item (variable)
            (let* ((name          (jenkins.model.variables:variable-info-name variable))
                   (type          (jenkins.model.variables:variable-info-type variable))
                   (documentation (jenkins.model.variables:variable-info-documentation variable))
                   (title         (string-downcase name))
                   (new-text      (typecase type
                                    ((eql string)
                                     (format nil "~(~A~): \"\""
                                             name))
                                    ((cons (eql jenkins.model::list-of) (cons (eql string) null))
                                     (format nil "~(~A~):~%- "
                                             name))
                                    (t
                                     (format nil "~(~A~): " name)))))
              (proto:make-completion-item
               title
               :kind          :variable
               :detail        (format nil "Type: ~A " type)
               :documentation documentation
               :range         (prefix-range context)
               :new-text      new-text)))))
    (loop :for variable :in (jenkins.model.variables:all-variables)
             #+no :when #+no (starts-with-subseq
                              prefix (string-downcase (jenkins.model.variables:variable-info-name variable)))
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

(defun remote-refs (project kind)
  (when-let ((repository (jenkins.model.variables:value/cast project :repository nil)))
    (mappend (lambda (line)
               (let+ (((&values match? groups)
                       (ppcre:scan-to-strings
                        (ecase kind
                          (:branch "refs/heads/(.*)$")
                          (:tag    "refs/tags/(.*)$"))
                        line)))
                 (when match?
                   (list (proto:make-completion-item
                          (aref groups 0)
                          :kind   :constant
                          :detail (format nil "Remote ~(~A~)" kind))))))
             (inferior-shell:run/lines `("git" "ls-remote" ,repository)))))

(defmethod contrib:completion-contributions
    ((workspace    t)
     (document     t)
     (context      variable-value-context)
     (contriubutor variable-value-completion-contributor))
  (let ((variable (variable-node context)))
    (cond ((member (jenkins.model.variables:variable-info-name variable) '(:branches :branch))
           (remote-refs (object document) :branch))
          ((member (jenkins.model.variables:variable-info-name variable) '(:tags :tag))
           (remote-refs (object document) :tag))
          (t
           (map 'list (lambda (value)
                        (proto:make-completion-item value :kind :constant))
                (possible-values (jenkins.model.variables:variable-info-type variable)))))))

;;;

(defclass project-name-completion-contributor () ())

(defun describe-project (project)
  (let ((natures     (jenkins.model.variables:value/cast
                      project :natures '()))
        (programming-languages     (jenkins.model.variables:value/cast
                      project :programming-languages '()))
        (licenses    (jenkins.model.variables:value/cast
                       project :licenses '()))
        (maintainers (ensure-list
                      (jenkins.model.variables:value/cast
                       project :recipe.maintainer '())))
        (description (jenkins.model.variables:value/cast
                      project :description "«no description»")))
    (format nil "Nature: ~A~%~
                 Programming Languages: ~A~%~
                 License: ~{~A~^ ~}~%~
                 Maintainer~P:~%~
                 ~{* ~A~^~%~}~
                 ~2%~A"
            natures
            programming-languages
            licenses
            (length maintainers) maintainers
            description)))

(defmethod contrib:completion-contributions
    ((workspace   t)
     (document    t)
     (context     project-name-context)
     (contributor project-name-completion-contributor))
  (let ((prefix   (prefix context))
        (projects (projects (workspace document))))
    (when (lparallel:fulfilledp projects)
     (mapcan (lambda (project)
               (let ((name (jenkins.model:name project)))
                 (when (starts-with-subseq prefix name)
                   (list (proto:make-completion-item
                          name
                          :kind          :file
                          :detail        "project"
                          :documentation (describe-project project))))))
             (lparallel:force projects)))))

;;;

(defclass aspect-class-completion-contributor () ())

(defmethod contrib:completion-contributions
    ((workspace   t)
     (document    t)
     (context     aspect-class-context)
     (contributor aspect-class-completion-contributor))
  (protocol.language-server::debug1 context)
  (let ((prefix ""                      ;(prefix context)
                ))
    (mappend (lambda (provider)
               (let ((name (string-downcase (service-provider:provider-name provider))))
                 (when (starts-with-subseq prefix name)
                   (list (proto:make-completion-item
                          name :kind :class :detail "aspect")))))
             (service-provider:service-providers 'jenkins.model.aspects::aspect))))

;;;

(defclass document ()
  ((index :initarg :index
          :reader  index)))

#+no (let* ((source   (text.source-location:make-source "<string>"))
            (index    (text.source-location.lookup:make-index))
            (document (make-instance 'document :index index)))
       (values (language.yaml:load "variables:
  foo: bar"
                                   :builder (jenkins.model.project::make-builder
                                             source index))
               index
               (lookup:lookup (text.source-location:make-location source 13 13) index)
               (contrib::context-contributions
                t
                document
                (text.source-location:make-location source 13 13)
                (make-instance 'variable-name-context-contributor))))
