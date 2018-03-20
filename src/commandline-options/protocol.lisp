;;;; protocol.lisp --- Protocol provided by the commandline-options module.
;;;;
;;;; Copyright (C) 2017, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.project.commandline-options)

;;; Option info protocol

(defgeneric option-multiplicity (info)
  (:documentation
   "Return 1 or `*' indicating the multiplicity of INFO."))

(defgeneric option-value (info index designator included-value maybe-value)
  (:documentation
   "Return value and number of consumed arguments for INFO.

    INDEX is a non-negative integer, the current position in the list
    of commandline arguments.

    DESIGNATOR is the string corresponding to the option name within
    the argument. Typically just the option value for positional
    options and of the form --LONG-NAME or -SHORT-NAME for named
    options.

    INCLUDED-VALUE is `nil' or a string corresponding to the option
    value within the argument. Typically VALUE in an argument of the
    form --LONG-NAME=VALUE.

    MAYBE-VALUE is the string following DESIGNATOR in the commandline
    argument list of `nil'.

    Return two values: 1) the option value 2) the number of consumed
    commandline arguments."))

(defgeneric option-synopsis (info stream &key long?)
  (:documentation
   "Write a representation of INFO's syntax to STREAM.

    The generalized Boolean LONG? controls whether an extended
    representation should be produced."))

;;; Default behavior

(defmethod option-multiplicity ((info t))
  (let+ (((&accessors-r/o option) info))
    (if (typep (configuration.options:option-type option)
               '(cons (eql list)))
        '*
        1)))

;;; Option contexts

(defvar *context-options* (make-hash-table :test #'equal)
  "Maps context names to option info objects.")

(defun find-options (context &key (if-does-not-exist #'error))
  (check-type context string)
  (let+ (((&values options options?) (gethash context *context-options*)))
    (if options?
        options
        (error-behavior-restart-case
            (if-does-not-exist (context-not-found-error
                                :context context))))))

(defun (setf find-options) (new-value context &key if-does-not-exist)
  (declare (ignore if-does-not-exist))
  (check-type context string)
  (setf (gethash context *context-options*) new-value))

(defun register-option (context option)
  (check-type context string)
  (setf (find-options context)
        (list* option (remove (designators option)
                              (find-options context
                                            :if-does-not-exist '())
                              :test #'equal :key #'designators))))

;;; Individual options

(defun find-option (designator context &key (if-does-not-exist #'error))
  (check-type designator option-designator)
  (or (when-let ((options (find-options
                           context :if-does-not-exist if-does-not-exist)))
        (flet ((find-by-designator (designator)
                 (find designator options
                       :test (rcurry #'find :test #'equal)
                       :key  #'designators)))
          (or (find-by-designator designator)
              (when (typep designator 'non-negative-integer)
                (find-by-designator '&rest)))))
      (error-behavior-restart-case
          (if-does-not-exist (option-not-found-error
                              :context context
                              :option  designator)))))

(defun value-for-option (context index option-designator maybe-value)
  (check-type option-designator string)
  (let+ (((&values key designator included-value)
          (if (named-option-designator? option-designator)
              (if-let ((index (position #\= option-designator)))
                (values (subseq option-designator 0 index)
                        (subseq option-designator 0 index)
                        (subseq option-designator (1+ index)))
                (values option-designator option-designator))
              (values index option-designator)))
         (info         (find-option key context))
         (option-name  (configuration.options:option-name
                        (option info)))
         (option-path  (configuration.options:merge-names
                        (list context) option-name))
         (multiplicity (option-multiplicity info))
         ((&values value consumed-count)
          (option-value info index designator included-value maybe-value)))
    (values info option-path multiplicity value consumed-count)))

;;; High-level interface

(defun map-commandline-options (function context arguments
                                &key stop-at-positional?)
  "Call function with option values according to CONTEXT and ARGUMENTS.

   The lambda-list of FUNCTION has to be compatible with

     (option-path value)

   where OPTION-PATH is the path of the option within the sub-schema
   indicated by CONTEXT and VALUE is the (raw, i.e. string) value of
   the option.

   CONTEXT names a sub-schema of the global configuration that should
   be used for interpreting ARGUMENTS.

   ARGUMENTS is the raw list of supplied commandline arguments.

   STOP-AT-POSITIONAL? controls the behavior in case a argument /not/
   of the form -SHORT-NAME or --LONG-NAME[=VALUE] is encountered
   before the end of ARGUMENTS. If true, processing stops at that
   argument instead of treating it as a positional argument.

   Return the number of elements consumed from ARGUMENTS."
  (let+ ((infos   (find-options context))
         (missing (copy-list (remove-if-not #'mandatory? infos)))
         (values  (make-hash-table :test #'equal))
         ((&flet add-value (option-path value multiplicity)
            (ecase multiplicity
              (1
               (setf (gethash option-path values) value))
              (*
               (appendf (gethash option-path values '()) (list value))))
            (log:debug "~@<Got option ~S value ~S with ~S (result ~S)~@:>"
                       option-path value multiplicity
                       (gethash option-path values)))))
    (prog1
        ;; Collect option values.
        (loop :with arguments = arguments
           :with consumed-total = 0
           :with position = 0
           :while (and arguments
                       (or (not stop-at-positional?)
                           (named-option-designator? (first arguments))))
           :for (option-designator maybe-value) = arguments
           :for (info option-path multiplicity value consumed-count)
             = (multiple-value-list
                (value-for-option
                 context position option-designator maybe-value))
           :do
             (removef missing info :test #'eq)
             (add-value option-path value multiplicity)
             (setf arguments (nthcdr consumed-count arguments))
             (incf consumed-total consumed-count)
             (when (typep info 'positional-option-info)
               (incf position consumed-count))
           :finally (return (when arguments consumed-total)))
      ;; Apply collected values.
      (when missing
        (error 'mandatory-options-not-supplied-error
               :context context
               :missing missing))
      (maphash (curry #'funcall function) values))))
