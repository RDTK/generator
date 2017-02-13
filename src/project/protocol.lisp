;;;; protocol.lisp --- Protocol provided by the project module.
;;;;
;;;; Copyright (C) 2012-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.project)

;;; Dependencies protocol

(defgeneric direct-dependencies (thing)
  (:documentation
   "Return a list of direct (as opposed to transitive) dependencies of
    THING."))

(defgeneric dependencies (thing)
  (:documentation
   "Return a duplicate-free list of transitive dependencies of
    THING."))

(defgeneric minimal-dependencies (thing)
  (:documentation
   "Return a list of direct dependencies of THING that are not also
    transitive dependencies of the direct dependencies of THING."))

;; Default behavior

(defmethod dependencies ((thing t))
  (let+ ((result '())
         ((&labels one-thing (thing)
            (dolist (dependency (direct-dependencies thing))
              (unless (member dependency result :test #'eq)
                (push dependency result)
                (one-thing dependency))))))
    (one-thing thing)
    result))

(defmethod minimal-dependencies ((thing t))
  (let* ((direct-dependencies (direct-dependencies thing))
         (indirect            (reduce #'union direct-dependencies
                                      :key           #'dependencies
                                      :initial-value '())))
    (set-difference direct-dependencies indirect)))

;;; Variables protocol

(defgeneric direct-variables (thing)
  (:documentation
   "Return a plist of the variables stored in THING.

    The returned plist does not include variables THING inherited from
    other objects.

    See `variables'."))

(defgeneric variables (thing)
  (:method-combination append)
  (:documentation
   "Return a plist of all the variables provided by THING.

    The returned plist may include variables THING inherited from
    other objects.

    See `direct-variables'."))

(defgeneric lookup (thing name &key if-undefined)
  (:documentation
   "Return two values:

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

;; Default behavior

(defmethod lookup :around ((thing t) (name t)
                           &key
                           (if-undefined #'error))
  (let+ (((&values value more-values found?) (call-next-method)))
    (if found?
        (values value more-values found?)
        (error-behavior-restart-case
            (if-undefined (simple-error
                           :format-control   "~@<Undefined variable: ~S.~@:>"
                           :format-arguments (list name)))))))

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

;;; Platform requirements protocol

(defgeneric platform-requires (object platform)
  (:documentation
   "Return a list of \"platform requirements\" for OBJECT and PLATFORM
    with elements of the form

      (NAME VERSION)"))

(defmethod platform-requires ((object t) (platform cons))
  (let+ ((spec (as (value object :platform-requires '()) 'list))
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
  (mapcar (lambda+ ((nature name &optional version))
            `((,(make-keyword (string-upcase nature))
                ,name
                ,@(when version `(,(parse-version version))))
              .
              :system-package))
          (as (value object :platform-provides '()) 'list)))

;;; Access protocol

(defgeneric access (object)
  (:documentation
   "Return the access specification for OBJECT, either :private
    or :public."))

(defgeneric check-access (object lower-bound)
  (:documentation
   "Return true if OBJECT permits at least the level of access
    indicates by LOWER-BOUND. If OBJECT does not permit the access,
    return nil and a optionally a condition instance describing the
    reason as a second value."))

(defmethod access ((object t))
  (switch ((as (value object :access nil) '(or null string)) :test #'equal)
    (nil       :public)
    ("public"  :public)
    ("private" :private)))

(defmethod check-access ((object t) (lower-bound t))
  t)

(defmethod check-access ((object t) (lower-bound (eql :public)))
  (eq (access object) lower-bound))

;;; Instantiation protocol

(defgeneric instantiate? (spec parent)
  (:documentation
   "Return non-nil when SPEC should be instantiated."))

(defgeneric instantiate (spec &key parent specification-parent)
  (:documentation
   "Instantiate the specification SPEC and return the created
    object.

    Signal `instantiation-condition's such as `instantiation-error'
    when conditions such as errors are encountered."))

(defgeneric add-dependencies! (thing spec &key providers)
  (:documentation
   "TODO(jmoringe): document"))

;; Default behavior

(defmethod instantiate? ((spec t) (parent t))
  t)

(defmethod instantiate :around ((spec t) &key parent specification-parent)
  (declare (ignore parent specification-parent))
  (with-condition-translation
      (((error instantiation-error)
        :specification spec))
    (with-simple-restart (continue "~@<Skip instantiation of ~A.~@:>" spec)
      (let ((implementation (call-next-method)))
        (setf (%specification implementation) spec)
        (push implementation (%implementations spec))
        (assert implementation)
        implementation))))

(defmethod add-dependencies! :around ((thing t) (spec t)
                                      &key providers)
  (declare (ignore providers))
  (with-condition-translation
      (((error instantiation-error)
        :specification spec))
    (with-simple-restart (continue "~@<Skip adding dependencies to ~A ~
                                    according to ~A.~@:>"
                                   thing spec)
      (call-next-method))))

;;; Aspect protocol

(defgeneric aspect< (left right)
  (:documentation
   "Return non-nil if the aspect LEFT should be applied before the
    aspect RIGHT."))

(defgeneric builder-constraints (aspect builder)
  (:documentation
   "Return a list of ordering constraints of one of the forms

      (:before (t | TAG) )
      (:after  (t | TAG) )

    for BUILDER created and configured by ASPECT."))

(defgeneric builder< (left right constraints)
  (:documentation
   "Return non-nil if CONSTRAINTS mandate that the builder LEFT should
    be executed before the builder RIGHT."))

(defgeneric extend! (job aspect spec)
  (:method-combination progn))

;; Default behavior

(defvar *builder-constraints* nil)

(defmethod extend! progn ((job t) (aspect list) (spec t))
  ;; Apply aspects, respecting declared ordering, and sort generated
  ;; builders according to declared ordering.
  (let ((*builder-constraints* (make-hash-table))
        (aspects (sort-with-partial-order (copy-list aspect) #'aspect<)))
    ;; Methods on `extend!' add entries to `*builder-constraints*'
    ;; and push builders onto (builders job).
    (reduce (rcurry #'extend! spec) aspects :initial-value job)

    (log:trace "Builder constraints: ~S"
               (hash-table-alist *builder-constraints*))

    ;; Try to sort builders according to `*builder-constraints*'.
    (setf (builders job)
          (sort-with-partial-order
           (builders job) (rcurry #'builder< *builder-constraints*)))

    (log:trace "Sorted builders: ~A" (builders job))))

;;; Deployment protocol

(defgeneric deploy (thing)
  (:documentation
   "Deploy THING .

    Signal `deployment-condition's such as `deployment-error' when
    conditions such as errors are encountered."))

(defgeneric deploy-dependencies (thing)
  (:documentation
   "TODO(jmoringe): document"))

;; Default behavior

(defmethod deploy :around ((thing t))
  (with-condition-translation
      (((error deployment-error)
        :thing thing))
    (with-simple-restart (continue "~@<Skip deployment of ~A.~@:>" thing)
      (call-next-method))))

(defmethod deploy-dependencies :around ((thing t))
  (with-condition-translation
      (((error deployment-error)
        :thing thing))
    (with-simple-restart (continue "~@<Skip deploying dependencies of ~
                                    ~A.~@:>"
                                   thing)
      (call-next-method))))
