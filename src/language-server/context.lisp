;;;; context.lisp --- Context contributors for different recipe types.
;;;;
;;;; Copyright (C) 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:build-generator.language-server)

;;; Utilities

(defun structure-path (position document)
  (when-let* ((locations (lookup:lookup position (index document))))
    (let ((path (mappend
                 (lambda (node)
                   (when (typep node '(cons keyword))
                     (list (car node))))
                 (map 'list (lambda (location)
                              (jenkins.model.project::object-at
                               location (locations document)))
                      locations))))
      ;; TODO removing duplicates should not be necessary
      (values (remove-duplicates path :test #'equal) locations))))

;;; `structure-context'

(defclass structure-context (print-items:print-items-mixin)
  ((%path :initarg :path
          :reader  path)))

(defmethod print-items:print-items append ((object structure-context))
  `((:path ,(reverse (path object)) "~{~A~^ » ~}")))

(defclass structure-context-contributor () ())

(defmethod contrib:context-contributions
    ((workspace   t)
     (document    t)
     (position    t)
     (contributor structure-context-contributor))
  (when-let* ((path (structure-path position document)))
    (list (make-instance 'structure-context :path path))))

;;;

(defclass template-name-context () ())

(defclass template-name-context-contributor () ())

(defmethod contrib:context-contributions
    ((workspace   t)
     (document    project-document)
     (position    t)
     (contributor template-name-context-contributor))
  (when-let* ((path  (structure-path position document))
              (depth (position :templates path)))
    (when (eql depth 0)
      (list (make-instance 'template-name-context)))))

(defmethod contrib:context-contributions
    ((workspace   t)
     (document    template-document)
     (position    t)
     (contributor template-name-context-contributor))
  (when-let* ((path  (structure-path position document))
              (depth (position :inherit path)))
    (when (eql depth 0)
      (list (make-instance 'template-name-context)))))

;;;

(defclass variable-name-context (print-items:print-items-mixin)
  ((%prefix       :initarg :prefix
                  :reader  prefix)
   (%prefix-range :initarg :prefix-range
                  :reader  prefix-range)))

(defmethod print-items:print-items append ((object variable-name-context))
  `((:prefix ,(prefix object) "~S")))

(defclass variable-name-context-contributor () ())

(defmethod contrib:context-contributions
    ((workspace    t)
     (document     t)
     (position     t)
     (contriubutor variable-name-context-contributor))
  (when-let* ((locations (lookup:lookup position (index document)))
              (leaf      (jenkins.model.project::object-at (first locations) (locations document)))
              (leaf      (when (stringp leaf)
                           leaf))
              (path      (structure-path position document))
              (position* (position :variables path)))
    (cond ((and (= position* 1)
                (string-equal leaf (first path))) ;; TODO could be deeper within dictionary value
           (list (make-instance 'variable-name-context
                                :prefix       (string leaf)
                                :prefix-range (sloc:range (first locations)))))

          ((stringp leaf)
           (let ((relative (- (sloc:index position) (sloc:index (sloc:start (first locations))))))
             (log:error leaf (sloc:start (first locations)) relative)
             (when (search "${" leaf :end2 (1+ relative) :from-end t)
               (list (make-instance 'variable-name-context
                                    :prefix       leaf
                                    :prefix-range (sloc:range (first locations))))))))))

;;;

(defclass variable-value-context ()
  ((%variable-location :initarg :variable-location
                       :reader  variable-location)
   (%variable-node     :initarg :variable-node
                       :reader  variable-node)
   (%prefix-range      :initarg :prefix-range
                       :reader  prefix-range)))

(defclass variable-value-context-contributor () ())

(defmethod contrib:context-contributions
    ((workspace    t)
     (document     t)
     (position     t)
     (contriubutor variable-value-context-contributor))
  (let+ (((&values path (&optional location &rest &ign))
          (structure-path position document)))
    (when-let ((position (position :variables path)))
      (when (= position 1)
        (when-let* ((name     (first path))
                    (variable (var:find-variable name :if-does-not-exist nil)))
          (log:error (list path position name variable location))
          (list (make-instance 'variable-value-context
                               :variable-location location ; TODO
                               :variable-node     variable
                               ; :prefix-range
                               )))))))

;;; project version reference context provider

(defclass project-name-context (print-items:print-items-mixin)
  ((%prefix :initarg :prefix
            :reader  prefix)))

(defmethod print-items:print-items append ((object project-name-context))
  `((:prefix ,(prefix object) "~S")))

(defclass project-version-context () ())

(defclass project-version-reference-context-contributor () ())

(defmethod contrib:context-contributions
    ((workspace   t)
     (document    t)
     (position    t)
     (contributor project-version-reference-context-contributor))
  (let+ (((&values path (&optional location &rest &ign))
          (structure-path position document))
         (thing (jenkins.model.project::object-at location (locations document))))
    (log:error path location thing)
    (when-let ((position (position :versions path)))
      (cond ((and (= position 0) (stringp thing) (not (find #\@ thing)))
             (list (make-instance 'project-name-context :prefix thing)))))))

;;; Aspect name context

(defclass aspect-class-context ()
  ((%prefix :initarg :prefix
            :reader  prefix)))

(defclass aspect-class-context-contributor () ())

(defmethod contrib:context-contributions
    ((workspcae   t)
     (document    t)
     (position    t)
     (contributor aspect-class-context-contributor))
  (let+ (((&values path (&optional location &rest &ign))
          (structure-path position document))
         (thing (jenkins.model.project::object-at location (locations document))))
    (log:error path location thing)
    (when (ends-with-subseq '(:aspect :aspects) path)
      (list (make-instance 'aspect-class-context :prefix thing)))))
