;;;; protocol.lisp --- Protocol provided by the project module.
;;;;
;;;; Copyright (C) 2012, 2013, 2014, 2015, 2016 Jan Moringen
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

;; Default behavior

(defmethod lookup ((thing t) (name t)
                   &key
                   if-undefined)
  (declare (ignore if-undefined))
  (when-let ((cells (remove name (variables thing)
                            :test (complement #'eq)
                            :key  #'car)))
    (values (first cells) (rest cells) t)))

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

(defun maybe-first (thing)
  (if (and (length= 1 thing)
           (typep (first thing) '(or string (cons (member :ref :ref/list)))))
      (first thing)
      thing))

(esrap:defrule escaped-syntactic-character
    (and #\\ (or #\$ #\@ #\}))
  (:function second))

(esrap:defrule uninterpreted-$-or-@
    (and (or #\$ #\@) (esrap:! #\{))
  (:function first))

(esrap:defrule text
    (+ (or escaped-syntactic-character
           uninterpreted-$-or-@
           (not (or #\$ #\@))))
  (:text t))

(esrap:defrule variable-reference/content
    (+ (not (or #\| #\} #\$ #\@)))
  (:text t))

(esrap:defrule text/ended-by-}
    (and (+ (or escaped-syntactic-character (not (or #\$ #\@ #\}))))
         (esrap:& #\}))
  (:function first)
  (:text t))

(esrap:defrule text/not-started-by-{
    (and (esrap:! #\}) text)
  (:function second))

(esrap:defrule reference-expr
    (+ (or variable-reference/content variable-reference)))

(esrap:defrule default-expr
    (and (+ (or variable-reference text/ended-by-} text/not-started-by-{))
         (esrap:& #\}))
  (:function first)
  (:function maybe-first))

(esrap:defrule variable-reference
    (and (or #\$ #\@) #\{
         reference-expr (esrap:? (and #\| (esrap:? default-expr)))
         #\})
  (:destructure (kind open content default close)
    (declare (ignore open close))
    (let ((default (when default
                     (list :default (unless (equal (second default) "[]")
                                      (second default))))))
     (cond
       ((string= kind "$")
        (list* :ref content default))
       ((string= kind "@")
        (list* :ref/list content default))))))

(esrap:defrule expr
    (* (or variable-reference text)))

(defun parse (expr &key (parse-strings? t))
  (let+ (((&labels recur (expr)
            (etypecase expr
              (string
               (if parse-strings?
                   (let ((result (esrap:parse 'expr expr)))
                     (if (length= 1 result)
                         (first result)
                         result))
                   expr))
              ((cons (cons keyword (not cons)))
               (list* :alist (mapcar (lambda+ ((key . value))
                                       (cons key (recur value)))
                                     expr)))
              (list
               (list* :list (mapcar #'recur expr)))
              (t
               expr)))))
    (recur expr)))

(mapc (lambda+ ((input expected))
        (assert (equal (parse input) expected)))
      '((""          nil)
        ("foo"       "foo")
        ("foo$bar"   "foo$bar")
        ("foo{bar"   "foo{bar")
        ("foo}bar"   "foo}bar")

        ("${a}"      (:ref ("a")))
        ("${a|}"     (:ref ("a") :default nil))
        ("${a|b}"    (:ref ("a") :default "b"))
        ("${a|{b}}"  ((:ref ("a") :default "{b") "}"))
        ("${a|${b}}" (:ref ("a") :default (:ref ("b"))))
        ("${${a}}"   (:ref ((:ref ("a")))))
        ("@{a}"      (:ref/list ("a")))
        ("@{a|}"     (:ref/list ("a") :default nil))
        ("@{a|b}"    (:ref/list ("a") :default "b"))
        ("@{a|{b}}"  ((:ref/list ("a") :default "{b") "}"))
        ("@{a|${b}}" (:ref/list ("a") :default (:ref ("b"))))
        ("@{@{a}}"   (:ref/list ((:ref/list ("a")))))

        ("foo${a}"   ("foo" (:ref ("a"))))))

(declaim (special *seen*))

(defvar *seen* nil
  "TODO(jmoringe): document")

(defun expand (pattern lookup)
  (let+ (((&flet lookup (name &optional (default nil default-supplied?))
            (let+ ((name (make-keyword (string-upcase name)))
                   ((&values value parsed? defaulted?)
                    (if default-supplied?
                        (funcall lookup name default)
                        (funcall lookup name))))
              (if defaulted?
                  value
                  (parse value :parse-strings? (not parsed?))))))
         ((&labels collapse (thing)
            (cond
              ((typep thing '(or number string boolean))
               thing)
              ((every #'stringp thing)
               (esrap:text thing))
              ((every #'listp thing)
               (reduce #'append thing))
              (t
               (apply #'map-product (compose #'collapse #'list)
                      (mapcar #'ensure-list thing))))))
         ((&labels recur (pattern)
            (optima:ematch pattern
              ((list (or :ref :ref/list) (optima:guard pattern (stringp pattern))
                     :default default)
               (recur (lookup pattern default)))

              ((list (or :ref :ref/list) (optima:guard pattern (stringp pattern)))
               (recur (lookup pattern)))

              ((list* :ref pattern rest)
               (recur (list* :ref (first (recur pattern)) rest)))

              ((list* :ref/list pattern rest)
               (first (recur (list* :ref/list (first (recur pattern)) rest))))

              ((optima:guard pattern (atom pattern))
               (list pattern))

              ((list* :list subpatterns)
               (list (mappend #'recur subpatterns)))

              ((list* :alist subpatterns)
               (list (mapcar (lambda+ ((key . value))
                               (cons key (first (recur value))))
                             subpatterns)))

              ((list* first rest)
               (let ((result (mappend #'recur (cons first rest))))
                 (list (collapse result))))))))
    (first (recur pattern))))

(defmethod value ((thing t) (name t) &optional (default nil default-supplied?))
  (let+ (((&values raw raw/next-values defined?)
          (lookup thing name
                  :if-undefined (unless default-supplied? #'error)))
         ((&labels+ make-lookup ((&optional first-value &rest next-values))
            (lambda (name1 &optional (default nil default-supplied?))
              (cond
                ((not (eq name1 :next-value))
                 (if default-supplied?
                     (let+ (((&values value defaulted?) (value thing name1 default)))
                       (values value t defaulted?))
                     (values (value thing name1) t)))
                (first-value
                 (with-augmented-trace (name1 nil first-value)
                   (values (expand (parse (cdr first-value))
                                   (make-lookup next-values))
                           t)))
                (default-supplied?
                 (with-augmented-trace (name1 :default (cons :unused default))
                   (values default t t)))
                (t
                 (error "~@<No next value for ~A.~@:>"
                        name)))))))
    (with-augmented-trace (name thing raw)
      (if defined?
          (expand (parse (cdr raw)) (make-lookup raw/next-values))
          (values default t)))))

;;; Platform requirements protocol

(defgeneric platform-requires (object platform)
  (:documentation
   "Return a list of \"platform requirements\" for OBJECT and PLATFORM
    with elements of the form

      (NAME VERSION)"))

(defmethod platform-requires ((object t) (platform cons))
  (let+ ((spec (value object :platform-requires '()))
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
  (switch ((value object :access nil) :test #'equal)
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

(defgeneric instantiate (spec)
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

(defmethod instantiate :around ((spec t))
  (with-condition-translation
      (((error instantiation-error)
        :specification spec))
    (restart-case
        (let ((implementation (call-next-method)))
          (setf (%specification implementation) spec)
          (push implementation (%implementations spec))
          (assert implementation)
          implementation)
      (continue (&optional condition)
        :report (lambda (stream)
                  (format stream "~@<Skip instantiation of ~A.~@:>"
                          spec))
        (declare (ignore condition))
        nil))))

(defmethod add-dependencies! :around ((thing t) (spec t)
                                      &key providers)
  (declare (ignore providers))
  (with-condition-translation
      (((error instantiation-error)
        :specification spec))
    (restart-case (call-next-method)
      (continue (&optional condition)
        :report (lambda (stream)
                  (format stream "~@<Skip adding dependencies to ~A ~
                                  according to ~A.~@:>"
                          thing spec))
        (declare (ignore condition))
        nil))))

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
    (restart-case (call-next-method)
      (continue (&optional condition)
        :report (lambda (stream)
                  (format stream "~@<Skip deployment of ~A.~@:>"
                          thing))
        (declare (ignore condition))
        nil))))

(defmethod deploy-dependencies :around ((thing t))
  (with-condition-translation
      (((error deployment-error)
        :thing thing))
    (restart-case (call-next-method)
      (continue (&optional condition)
        :report (lambda (stream)
                  (format stream "~@<Skip deploying dependencies of ~
                                  ~A.~@:>"
                          thing))
        (declare (ignore condition))
        nil))))
