;;;; options.lisp --- Option info classes use in the commandline-options module.
;;;;
;;;; Copyright (C) 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.project.commandline-options)

;;; `option-info'

(defclass option-info ()
  ((option        :initarg  :option
                  :reader   option)
   (designators   :initarg  :designators
                  :type     list
                  :reader   designators)
   (argument-name :initarg  :argument-name
                  :type     (or null string)
                  :reader   argument-name)
   (mandatory?    :initarg  :mandatory?
                  :type     boolean
                  :reader   mandatory?
                  :initform nil)))

;;; `named-without-argument-option-info'

(defclass named-without-argument-option-info (option-info)
  ())

(defmethod option-value ((info           named-without-argument-option-info)
                         (index          integer)
                         (designator     string)
                         (included-value t)
                         (maybe-value    t))
  (if included-value
      (error "~@<The \"~A\" option does not take an argument.~@:>"
             designator)
      (values t 1)))

;;; `named-with-argument-option-info'

(defclass named-with-argument-option-info (option-info)
  ())

(defmethod option-value ((info           named-with-argument-option-info)
                         (index          integer)
                         (designator     string)
                         (included-value t)
                         (maybe-value    t))
  (let+ (((&accessors-r/o argument-name) info))
    (cond
      (included-value
       (values included-value 1))
      ((or (not maybe-value) (named-option-designator? maybe-value))
       (error "~@<The \"~A\" option requires a ~A argument.~@:>"
              designator argument-name))
      (t
       (values maybe-value 2)))))

;;; `positional-option-info'

(defclass positional-option-info (option-info)
  ())

(defmethod option-value ((info           positional-option-info)
                         (index          integer)
                         (designator     string)
                         (included-value t)
                         (maybe-value    t))
  (values designator 1))
