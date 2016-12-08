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

(defgeneric direct-dependencies/reasons (thing)
  (:documentation
   "Return an alist of dependency target-reasons pairs.

    That is, elements are of the form

      (TARGET . REASONS)

    where TARGET is the object upon which THING depends and REASONS is
    a list of requirement specifications that are satisfied by things
    TARGET provides."))

;;; Person container protocol

(defgeneric persons (container)
  (:documentation
   "Return all person objects in CONTAINER."))

(defgeneric persons-in-roles/plist (container)
  (:documentation
   "Return a plist of roles and person object lists in CONTAINER."))

(defgeneric persons-in-role (role container)
  (:documentation
   "Return a list of person objects for ROLE in CONTAINER."))

;;; Platform-specific value protocol

(defgeneric platform-specific-value (spec platform name &key merge))

(defmethod platform-specific-value
    ((spec t) (platform cons) (name string)
     &rest args &key merge)
  (declare (ignore merge))
  (let ((variable-name (make-keyword (string-upcase name))))
    (apply #'platform-specific-value spec platform variable-name args)))

(defmethod platform-specific-value
    ((spec t) (platform cons) (name symbol)
     &key
     (merge (lambda (values)
              (remove-duplicates (reduce #'append values :from-end t)
                                 :test #'string=))))
  (let+ (((&flet lookup (name &optional (where spec))
            (cdr (assoc name where :test #'eq))))
         ((&flet make-key (string)
            (json:json-intern (json:camel-case-to-lisp string))))
         ((&labels+ collect (spec (&optional platform-first &rest platform-rest))
            (list*
             (lookup name spec)
             (when platform-first
               (when-let ((child (lookup (make-key platform-first) spec)))
                 (collect child platform-rest)))))))
    (funcall merge (collect spec platform))))

;;; Platform requirements protocol

(defgeneric platform-requires (object platform)
  (:documentation
   "Return a list of \"platform requirements\" for OBJECT and PLATFORM
    with elements of the form

      (NAME VERSION)"))

(defmethod platform-requires ((object t) (platform cons))
  (let ((spec (value/cast object :platform-requires '())))
    (platform-specific-value spec platform :packages)))

(defmethod platform-requires ((object sequence) (platform cons))
  (let ((requirements (mappend (rcurry #'platform-requires platform) object)))
    (remove-duplicates requirements :test #'string=)))

(defun platform-provides (object)
  (mapcar (lambda (spec)
            `(,(parse-dependency-spec spec) . :system-package))
          (value/cast object :platform-provides '())))
