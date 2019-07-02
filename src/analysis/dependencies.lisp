;;;; dependencies.lisp --- Dependency-related functions.
;;;;
;;;; Copyright (C) 2013-2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.analysis)

;;; Natures and targets

(defun same-target? (left-nature left-target right-nature right-target)
  (and (eq left-nature right-nature)
       (or (string= left-target right-target)
           (and (eq left-nature :cmake)
                (string-equal left-target right-target)))))

(defun target-matches? (required-nature required-target
                        provided-nature provided-target)
  (same-target? required-nature required-target
                provided-nature provided-target))

(defun+ dependency-matches?
    ((required-nature required-target &optional required-version)
     (provided-nature provided-target &optional provided-version))
  (and (target-matches? required-nature required-target
                        provided-nature provided-target)
       (version:version-matches required-version provided-version)))

;;; Dependencies

(defun effective-requires (requires provides)
  (set-difference requires provides :test #'dependency-matches?))

(defun merge-dependencies (dependencies &key (test #'version:version>=))
  (let ((test (ensure-function test))
        (seen (make-hash-table :test #'equal)))
    (map nil (lambda+ ((&whole dependency nature name &optional version))
               (let ((key (cons nature (case nature
                                         (:cmake (string-downcase name))
                                         (t      name)))))
                 (setf (gethash key seen)
                       (let ((current (gethash key seen)))
                         (if (or (not current)
                                 (funcall test version (third current)))
                             dependency
                             current)))))
         dependencies)
    (hash-table-values seen)))

;;; Indexed dependencies

(defun dependency-key (dependency)
  (let+ (((nature target &optional version) dependency))
    (cond
      ((eq nature :cmake) (list nature (string-downcase target)))
      (version            (list nature target))
      (t                  dependency))))

(defun make-provider-index ()
  (make-hash-table :test #'equal))

(defun index-provider! (provided provider index)
  (let ((key (dependency-key provided)))
    (push (cons provided provider) (gethash key index '()))
    index))

(defun lookup-providers (required index)
  (gethash (dependency-key required) index))
