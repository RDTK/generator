;;;; variables.lisp --- Variables used by the project module.
;;;;
;;;; Copyright (C) 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.project)

(defmacro define-repository (name)
  "TODO(jmoringe): document"
  (let ((variable-name (format-symbol *package* "*~A~@(~P~)*" name 2))
        (lock-name     (format-symbol *package* "*~A~@(~P~)-LOCK*" name 2))
        (find-name     (format-symbol *package* "FIND-~A" name)))
   `(progn
      (defvar ,variable-name (make-hash-table :test #'equal)
        "TODO(jmoringe): document")

      (defvar ,lock-name (bt:make-lock)
        "TODO")

      (defun ,find-name (name &key (if-does-not-exist #'error))
        "TODO(jmoringe): document"
        (or (bt:with-lock-held (,lock-name)
              (gethash name ,variable-name))
            (etypecase if-does-not-exist
              (null
               if-does-not-exist)
              (function
               (funcall
                if-does-not-exist
                (make-condition
                 'simple-error
                 :format-control   ,(format nil "~~@<No such ~(~A~): ~~S.~~@:>"
                                            name)
                 :format-arguments (list name)))))))

      (defun (setf ,find-name) (new-value name &key if-does-not-exist)
        "TODO(jmoringe): document"
        (declare (ignore if-does-not-exist))

        (bt:with-lock-held (,lock-name)
          (setf (gethash name ,variable-name) new-value))))))

;;; Global template repository

(define-repository template)

;;; Global project repository with provider lookup

(define-repository project)

(define-repository provider)

(defun provider-version (spec)
  (third spec))

(defun push-provider (new-value name)
  (bt:with-lock-held (*providers-lock*)
    (push new-value (gethash name *providers* '()))))

(defun find-provider/version (spec
                              &key
                              (if-does-not-exist #'error)
                              order)
  (let+ (((mechanism name &optional version) spec)
         ((&flet version-better (left right)
            (cond
              ((equal version left)
               t)
              ((equal version right)
               nil)
              ((and (starts-with-subseq version left :test #'equal)
                    (not (starts-with-subseq version right :test #'equal)))
               t)
              ((starts-with-subseq version right :test #'equal)
               nil)
              ((version>= left right)))))
         ((&flet provider-better (left right)
            (or (and order (funcall order left right))
                (and (not (and order (funcall order right left)))
                     (version-better (provider-version (car left))
                                     (provider-version (car right)))))))
         (candidates (remove-if-not (lambda+ ((mechanism1 name1 &optional version1))
                                      (and (eq              mechanism1 mechanism)
                                           (string=         name1      name)
                                           (version-matches version    version1)))
                                    (bt:with-lock-held (*providers-lock*)
                                      (hash-table-alist *providers*))
                                    :key #'car))
         (candidates
          (iter outer
                (for (version . providers) in candidates)
                (iter (for provider in providers)
                      (in outer (collect (cons version provider))))))
         (candidates (stable-sort candidates #'provider-better)))
    (or (cdr (first candidates))
        (etypecase if-does-not-exist
          (null
           if-does-not-exist)
          (function
           (funcall
            if-does-not-exist
            (make-condition
             'simple-error
             :format-control   "~@<No provider for ~S.~@[ Candidates: ~{~A~^, ~}~]~@:>"
             :format-arguments (list spec candidates))))))))

;;; Global instance registry

(define-repository instance)
