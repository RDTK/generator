;;;; builder.lisp --- Builder used for recipe concrete syntax.
;;;;
;;;; Copyright (C) 2016-2020 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:build-generator.model.project)

;;; YAML tags for includes

(language.yaml.tags:define-tag ("tag:build-generator,2018:include" :scalar))

(language.yaml.tags:define-tag ("tag:build-generator,2018:literal-include" :scalar))

;;; `recipe-builder'

(defclass recipe-builder (language.yaml.construct::native-builder)
  ((base-path :initarg  :base-path
              :reader   base-path)
   (root-path :initarg  :root-path
              :type     (or null
                            (and pathname (satisfies uiop:directory-pathname-p)))
              :reader   root-path
              :initform nil
              :documentation
              "Root path for root-relative includes.

               Usually root directory of the repository containing the
               recipe file that is being processed by the builder.

               Can be `nil', for example, when reading repository
               \"parents\" files."))
  (:default-initargs
   :base-path (missing-required-initarg 'recipe-builder :base-path)))

(defmethod initialize-instance :after ((instance recipe-builder) &key)
  (let ((expander (language.yaml.construct::expander instance)))
    (setf (language.yaml.tags:find-shorthand "b" expander)
          "tag:build-generator,2018:")))

(defmethod expand-pathname ((builder recipe-builder) (pathname string))
  (cond ((starts-with-subseq "//" pathname)
         (merge-pathnames (subseq pathname 2) (root-path builder)))
        ((starts-with-subseq "/" pathname)
         (pathname pathname))
        (t
         ;; Use only the directory component of the base path so that
         ;; `merge-pathnames' does not add type or version when
         ;; PATHNAME does not have them.
         (let ((base-path (uiop:pathname-directory-pathname
                           (base-path builder))))
           (merge-pathnames pathname base-path)))))

(defmethod language.yaml.construct::make-node-using-tag
    ((builder recipe-builder)
     (kind    (eql :scalar))
     (tag     (eql (language.yaml.tags:find-tag "tag:build-generator,2018:include")))
     &key
     content)
  (let ((filename (expand-pathname builder content)))
    (language.yaml:load filename :builder builder)))

(defun protect-string (string)
  (ppcre:regex-replace-all "\\${" string "\\${"))

(defmethod language.yaml.construct::make-node-using-tag
    ((builder recipe-builder)
     (kind    (eql :scalar))
     (tag     (eql (language.yaml.tags:find-tag "tag:build-generator,2018:literal-include")))
     &key
     content
     location)
  (handler-bind ((error (lambda (condition)
                          (setf (location-of content) location)
                          (object-error
                           `((,content "included here" :error))
                           "~@<Could not literally-include the ~
                            contents of \"~A\": ~A.~@:>"
                           content condition))))
    (let* ((filename  (expand-pathname builder content))
           (content   (read-file-into-string filename))
           (protected (protect-string content)))
      (architecture.builder-protocol:make-node builder :scalar
                                               :tag     "tag:yaml.org,2002:str"
                                               :content protected))))

(defun make-builder (source &key (root-path nil root-path-supplied?))
  (let* ((base-path (text.source-location:name source))
         (builder   (apply #'make-instance 'recipe-builder
                           :base-path base-path
                           (when root-path-supplied?
                             (list :root-path root-path)))))
    (make-instance 'text.source-location.source-tracking-builder::callback-source-tracking-builder
                   :target   builder
                   :source   source
                   :callback (lambda (object location)
                               (setf (location-of object) location)))))
