;;;; protocol.lisp --- Protocol provided by the project module.
;;;;
;;;; Copyright (C) 2012-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.model.project)

;;; Requires/provides protocol

(defgeneric requires (thing)
  (:documentation
   "Return a list of requirements for THING.

    Elements of the returned list are of the form

      (NATURE NAME [VERSION])

    ."))

(defgeneric requires-of-kind (nature thing)
  (:documentation
   "Return a list of requirements for THING restricted to NATURE.

    Natures designates a project nature such as `:asdf' or `:cmake'.

    Elements of the returned list are of the form

      (NATURE NAME [VERSION])

    i.e. the first element is NATURE."))

(defgeneric provides (thing)
  (:documentation
   "Return a list of things provided by THING.

    Elements of the returned list are of the form

      (NATURE NAME [VERSION])

    ."))

(defgeneric provides-of-kind (nature thing)
  (:documentation
   "Return a list of things provided by THING, restricted to NATURE.

    Natures designates a project nature such as `:asdf' or `:cmake'.

    Elements of the returned list are of the form

      (NATURE NAME [VERSION])

    i.e. the first element is NATURE."))

;;; Platform requirements protocol

(defgeneric platform-requires (object platform)
  (:documentation
   "Return a list of \"platform requirements\" for OBJECT and PLATFORM
    with elements of the form

      (NAME VERSION)"))

(defmethod platform-requires ((object t) (platform cons))
  (let+ ((spec (value/cast object :platform-requires '()))
         ((&flet lookup (name &optional (where spec))
            (cdr (assoc name where :test #'eq))))
         ((&flet make-key (string)
            (json:json-intern (json:camel-case-to-lisp string))))
         (requirements '())
         ((&labels+ collect (spec (&optional platform-first &rest platform-rest))
            (appendf requirements (lookup :packages spec))
            (when platform-first
              (when-let ((child (lookup (make-key platform-first) spec)))
                (collect child platform-rest))))))
    (collect spec platform)
    (remove-duplicates requirements :test #'string=)))

(defmethod platform-requires ((object sequence) (platform cons))
  (let ((requirements (mappend (rcurry #'platform-requires platform) object)))
    (remove-duplicates requirements :test #'string=)))

(defun platform-provides (object)
  (mapcar (lambda (spec)
            `(,(parse-dependency-spec spec) . :system-package))
          (value/cast object :platform-provides '())))
