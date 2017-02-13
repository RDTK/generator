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
                              (gethash location (location->object document)))
                      locations))))
      (values path locations))))

;;; `structure-context'

(defclass structure-context ()
  ((path :initarg :path
         :reader  path)))

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
     (document    t)
     (position    t)
     (contributor template-name-context-contributor))
  (when-let* ((path  (structure-path position document))
              (depth (position :templates path)))
    (when (= depth 0)
      (list (make-instance 'template-name-context)))))

;;;

(defclass variable-name-context () ())

(defclass variable-name-context-contributor () ())

(defmethod contrib:context-contributions
    ((workspace    t)
     (document     t)
     (position     t)
     (contriubutor variable-name-context-contributor))
  (when-let* ((locations (lookup:lookup position (index document)))
              (leaf      (gethash (first locations) (location->object document)))
              (leaf      (when (stringp leaf)
                           leaf))
              (path      (structure-path position document))
              (position*  (position :variables path)))
    (log:error locations leaf path position*)
    (cond ((and (= position* 1)
                (string-equal leaf (first path))) ;; TODO could be deeper within dictionary value
           (list (make-instance 'variable-name-context)))

          ((stringp leaf)
           (let ((relative (- (sloc:index position) (sloc:index (sloc:start (first locations))))))
             (log:error leaf (sloc:start (first locations)) relative)
             (when (search "${" leaf :end2 (1+ relative) :from-end t)
               (list (make-instance 'variable-name-context))))))))

;;;

(defclass variable-value-context ()
  ((%variable-location :initarg :variable-location
                       :reader  variable-location)
   (%variable-node     :initarg :variable-node
                       :reader  variable-node)))

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
          (log:error path position name variable)
          (list (make-instance 'variable-value-context
                               :variable-location location ; TODO
                               :variable-node     variable)))))))

;;; project version reference context provider

(defclass project-name-context ()
  ((%prefix :initarg :prefix
            :reader  prefix)))

(defclass project-version-context () ())

(defclass project-version-reference-context-contributor () ())

(defmethod contrib:context-contributions
    ((workspace   t)
     (document    t)
     (position    t)
     (contributor project-version-reference-context-contributor))
  (let+ (((&values path (&optional location &rest &ign))
          (structure-path position document))
         (thing (gethash location (location->object document))))
    (log:error path location thing)
    (when-let ((position (position :versions path)))
      (cond ((and (= position 0) (stringp thing) (not (find #\@ thing)))
             (list (make-instance 'project-name-context :prefix thing)))))))
