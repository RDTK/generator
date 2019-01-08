;;;; Protocol.lisp --- Protocol provided by the model.variables module.
;;;;
;;;; Copyright (C) 2012-2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.model.variables)

;;; Lookup protocol

(defgeneric direct-lookup (thing name)
  (:documentation
   "Lookup binding for NAME in THING.

    Return three values:

    1. the \"raw\" cell (a cons with the variable name in the `car'
       and the value in the `cdr') of the variable named NAME in THING
    2. an empty list
    3. a Boolean indicating whether THING has a value for NAME."))

(defgeneric lookup (thing name &key if-undefined)
  (:documentation
   "Return three values:

    1. the \"raw\" cell (a cons with the variable name in the `car'
       and the value in the `cdr') of the variable named NAME in THING
    2. a list of shadowed \"raw\" cells of NAME in THING
    3. a Boolean indicating whether THING has a value for NAME

    The returned cells are \"raw\" in the sense that substitutions of
    them forms ${next-value|NAME}, @{next-value|NAME}, etc. remain
    untouched. See `lookup'.

    Shadowed cells are introduced if a variable has one value in THING
    and other values in \"parents\" of THING which would be inherited
    if THING did not have the variable.

    IF-UNDEFINED controls the behavior in vase there is no variable
    named NAME in THING."))

(defgeneric (setf lookup) (new-value thing name &key if-undefined)
  (:documentation
   "Set the value of the variable named NAME in THING to NEW-VALUE.

    Doing this may shadow other values of NAME, see `lookup'.

    IF-UNDEFINED is accepted for parity with `lookup'."))

(defgeneric value (thing name &optional default)
  (:documentation
   "Return the \"resolved\" of the variable named NAME in THING.

    The return value is \"resolved\" in the sense that substitutions
    of them forms

      ${ ( next-value | NAME ) [ |DEFAULT ] }
      @{ ( next-value | NAME ) [ |[] ] }

    are recursively processed until none remain. See `lookup'.

    An error is signaled when processing a substitution fails.

    The second return value indicates whether the first return value
    is DEFAULT."))

(defgeneric evaluate (thing expression)
  (:documentation
   "Return the result of evaluating EXPRESSION in context THING.

    To produce the return value, substitutions of them forms

      ${ NAME [ |DEFAULT ] }
      @{ NAME [ |[] ] }

    are recursively processed until none remain. See `lookup'.

    An error is signaled when processing a substitution fails."))

(defgeneric as (value type &key if-type-mismatch)
  (:documentation
   "Interpret/convert VALUE as/to an element of TYPE and return that.

    Return two values: 1) VALUE or the result of converting VALUE to
    TYPE 2) T

    IF-TYPE-MISMATCH controls the behavior in case VALUE cannot be
    interpreted as/converted to an element of TYPE. Allowed values are

    'ERROR, #'ERROR

      Signal an error.

    NIL

      Return two values: VALUE and NIL."))

(defgeneric value/cast (thing name &optional default)
  (:documentation
   "Like `value' but cast returned value according to the type of NAME.

    If NAME does not name a defined variable, signal an error."))

(defgeneric aggregate-values (thing children name strategy)
  (:documentation
   "Aggregate values of NAME in THING and CHILDREN according to STRATEGY."))

;; Default behavior

(defmethod lookup :around ((thing t) (name t)
                           &key
                           (if-undefined #'error))
  (let+ (((&values value more-values defined?) (call-next-method)))
    (if defined?
        (values value more-values defined?)
        (if-let ((value
                  (error-behavior-restart-case
                      (if-undefined (undefined-variable-error :name name)))))
          (values value '() t)
          (values nil   '() nil)))))

(defmethod as :around ((value t) (type t) &key (if-type-mismatch #'error))
  (let+ (((&values result match?) (call-next-method value type)))
    (if match?
        (values result t)
        (error-behavior-restart-case
            (if-type-mismatch
             (simple-error
              :format-control   "~@<The value ~S cannot be interpreted ~
                                 as a value of type ~S.~@:>"
              :format-arguments (list value type)))))))

(defmethod as ((value t) (type t) &key if-type-mismatch)
  (declare (ignore if-type-mismatch))
  (when (typep value type)
    (values value t)))

;;; Container protocol

(defgeneric direct-variables (thing)
  (:documentation
   "Return a alist of the variables stored in THING.

    The returned alist does not include variables THING inherited from
    other objects.

    See `variables'."))

(defgeneric variables (thing)
  (:method-combination append)
  (:documentation
   "Return an alist of all the variables provided by THING.

    The returned alist may include variables THING inherited from
    other objects.

    See `direct-variables'."))
