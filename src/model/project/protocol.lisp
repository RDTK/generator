;;;; protocol.lisp --- Protocol provided by the project module.
;;;;
;;;; Copyright (C) 2012-2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:build-generator.model.project)

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

;;; Default behavior

(defmethod requires-of-kind ((nature t) (spec t))
  (remove nature (requires spec) :test-not #'eq :key #'first))

(defmethod provides-of-kind ((nature t) (spec t))
  (remove nature (provides spec) :test-not #'eq :key #'first))

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

;;; Platform requirements protocol

(defgeneric platform-requires (object platform)
  (:documentation
   "Return a list of \"platform requirements\" for OBJECT and PLATFORM
    with elements of the form

      (NAME VERSION)"))

(defmethod platform-requires :around ((object t) (platform cons))
  (remove-duplicates (call-next-method) :test #'string=))

(defmethod platform-requires ((object t) (platform cons))
  (let+ ((spec (var:value/cast object :platform-requires '()))
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
    requirements))

(defmethod platform-requires ((object sequence) (platform cons))
  (mappend (rcurry #'platform-requires platform) object))

(defun platform-provides (object)
  (mapcan (lambda (spec)
            (cond ;; New-style platform provides of the form:
                  ;;
                  ;; name: NAME
                  ;; variables:
                  ;;   platform-provides:
                  ;;
                  ((when-let* ((name      (assoc-value spec :name))
                               (variables (assoc-value spec :variables)))
                     (let ((dependency (make-platform-dependency name variables)))
                       (map 'list (rcurry #'cons dependency)
                            (provides dependency)))))
                  ;; Old-style platform provides.
                  (t
                   (list `(,(parse-dependency-spec spec) . :system-package)))))
          (var:value/cast object :platform-provides '())))

(defmethod var:value :around ((thing standard-object) (name (eql :repository)) &optional default)
  (declare (ignore default))
  (multiple-value-call #'apply-replacements
    :url-replacements (call-next-method)))
