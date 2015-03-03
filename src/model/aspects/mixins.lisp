;;;; mixins.lisp --- Mixin classes for aspect classes.
;;;;
;;;; Copyright (C) 2012, 2013, 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.model.aspects)

;;; `aspect-builder-defining-mixin'

(defclass aspect-builder-defining-mixin ()
  ()
  (:documentation
   "TODO"))

(defun+ parse-constraint ((&whole raw kind subject))
  (let+ (((&flet parse-kind ()
            (make-keyword (string-upcase kind))))
         ((&flet parse-class-or-tag (value)
            (if (or (equal value "<all>") (eq value t))
                t
                (intern (string-upcase value) #.*package*))))
         ((&flet parse-name (value)
            (if (or (equal value "<all>") (eq value t))
                t
                value))))
    (cond
      ((equal subject "<all>")
       (list (parse-kind) t t))
      ((stringp subject)
       (list (parse-kind) (parse-class-or-tag subject) t))
      ((consp subject)
       (let+ (((&plist-r/o (class-or-tag :type t) (name :name t))
               (alist-plist subject)))
         (list (parse-kind)
               (parse-class-or-tag class-or-tag)
               (parse-name name))))
      (t
       (error 'type-error
              :datum         raw
              :expected-type '(or (eql "<all>") cons))))))

(defmethod builder-constraints ((aspect  aspect-builder-defining-mixin)
                                (builder t))
  (let+ ((builder-type    (type-of builder))
         (variable        (format-symbol
                           :keyword "ASPECT.BUILDER-CONSTRAINTS.~@:(~A~)"
                           (let ((type-string (string builder-type)))
                             (subseq type-string (length "builder/")))))
         (constraints/raw (value aspect variable nil))
         (constraints     (mapcar #'parse-constraint constraints/raw)))
    (log:trace "Constraints for ~A in ~A: ~S" builder variable constraints)
    constraints))
