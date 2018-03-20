;;;; options.lisp --- Option info classes use in the commandline-options module.
;;;;
;;;; Copyright (C) 2017, 2018 Jan Moringen
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

(defmethod option-synopsis ((info   named-without-argument-option-info)
                            (stream t)
                            &key long?)
  (let+ (((&accessors-r/o designators) info))
    (format stream "~{~A~^,~}"
            (if long? designators (list (first designators))))))

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

(defmethod option-synopsis ((info   named-with-argument-option-info)
                            (stream t)
                            &key long?)
  (let+ (((&accessors-r/o option designators argument-name) info)
         (type (configuration.options:option-type option))
         ((&values default default?)
          (configuration.options:option-default
           option :if-does-not-exist nil))
         (default (when default?
                    (configuration.options:value->string
                     option default))))
    (format stream "~{~A~^,~}=~A"
            (if long? designators (list (first designators)))
            argument-name)
    (when long?
      (format stream " (~:[~*~;default: ~A, ~]~
                      type: ~A)"
              (and default? (not (eq (option-multiplicity info) '*))) default
              type))))

;;; `positional-option-info'

(defclass positional-option-info (option-info)
  ())

(defmethod option-value ((info           positional-option-info)
                         (index          integer)
                         (designator     string)
                         (included-value t)
                         (maybe-value    t))
  (values designator 1))

(defmethod option-synopsis ((info   positional-option-info)
                            (stream t)
                            &key long?)
  (let+ (((&accessors-r/o
           option argument-name mandatory? (multiplicity option-multiplicity))
          info)
         (type (configuration.options:option-type option)))
    (format stream "~:[[~A~@[*~]]~;~A~@[*~]~]"
            mandatory? argument-name (eq multiplicity '*))
    (when long?
      (format stream " (type: ~A)" type))))
