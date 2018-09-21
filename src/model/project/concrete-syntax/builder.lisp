;;;; builder.lisp --- Builder used for recipe concrete syntax.
;;;;
;;;; Copyright (C) 2016-2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:build-generator.model.project)

;;; YAML tags for includes

(language.yaml.tags:define-tag ("tag:build-generator,2018:include" :scalar))

(language.yaml.tags:define-tag ("tag:build-generator,2018:literal-include" :scalar))

;;; `recipe-builder'

(defclass recipe-builder (language.yaml.construct::native-builder)
  ((base-path :initarg :base-path
              :reader  base-path))
  (:default-initargs
   :base-path (missing-required-initarg 'recipe-builder :base-path)))

(defmethod initialize-instance :after ((instance recipe-builder) &key)
  (let ((expander (language.yaml.construct::expander instance)))
    (setf (language.yaml.tags:find-shorthand "b" expander)
          "tag:build-generator,2018:")))

(defmethod expand-pathname ((builder recipe-builder) (pathname t))
  (merge-pathnames pathname (base-path builder)))

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

(defun make-builder (source &optional index)
  (make-instance 'text.source-location.source-tracking-builder::callback-source-tracking-builder
                 :target   (make-instance 'recipe-builder
                                          :base-path (text.source-location:name source))
                 :source   source
                 :callback (lambda (object location)
                             (when index
                               (text.source-location.lookup:add! location index))
                             (setf (location-of object) location))))
