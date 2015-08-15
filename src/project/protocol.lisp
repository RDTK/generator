;;;; protocol.lisp --- Protocol provided by the project module.
;;;;
;;;; Copyright (C) 2012, 2013, 2014, 2015 Jan Moringen
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

(defgeneric lookup (thing name)
  (:documentation
   "Return two values:
    1. the \"raw\" value of the variable named NAME in THING
    2. a Boolean indicating whether THING has a value for NAME
    3. a list of shadowed \"raw\" values of NAME in THING

    The returned values are \"raw\" in the sense that substitutions of
    them forms ${next-value|NAME}, @{next-value|NAME}, etc. remain
    untouched. See `lookup'.

    Shadowed values are introduced if a variable has one value in
    THING and other values in \"parents\" of THING which would be
    inherited if THING did not have the variable."))

(defgeneric (setf lookup) (new-value thing name)
  (:documentation
   "Set the value of the variable named NAME in THING to NEW-VALUE.

    Doing this may shadow other values of NAME, see `lookup'."))

(defgeneric value (thing name)
  (:documentation
   "Return the \"resolved\" of the variable named NAME in THING.

    The returned values is \"resolved\" in the sense that
    substitutions of them forms

      ${ ( next-value | NAME ) [ |DEFAULT ] }
      @{ ( next-value | NAME ) [ |[] ] }

    are recursively processed until none remain. See `lookup'.

    An error is signaled when processing a substitution fails."))

;; Default behavior

(defmethod lookup ((thing t) (name t))
  (when-let ((cells (remove name (plist-alist (variables thing))
                            :test (complement #'eq)
                            :key  #'car)))
    (values (cdr (first cells)) t (mapcar #'cdr (rest cells)))))

(defmethod lookup :around ((thing t) (name t))
  (let+ (((&values value found? more-values) (call-next-method)))
    (if found?
        (values value more-values)
        (error "~@<Undefined variable: ~S.~@:>" name))))

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

(esrap:defrule default-expr
    (and (+ (or variable-reference text/ended-by-} text/not-started-by-{))
         (esrap:& #\}))
  (:function first))

(esrap:defrule variable-reference
    (and (or #\$ #\@) #\{
         (+ (or variable-reference/content variable-reference))
         (esrap:? (and #\| (esrap:? default-expr)))
         #\})
  (:destructure (kind open content default close)
    (declare (ignore open close))
    (let ((default (when default
                     (list :default (unless (equal (first (second default)) "[]")
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
        ("${a|b}"    (:ref ("a") :default ("b")))
        ("${a|{b}}"  ((:ref ("a") :default ("{b")) "}"))
        ("${a|${b}}" (:ref ("a") :default ((:ref ("b")))))
        ("${${a}}"   (:ref ((:ref ("a")))))
        ("@{a}"      (:ref/list ("a")))
        ("@{a|}"     (:ref/list ("a") :default nil))
        ("@{a|b}"    (:ref/list ("a") :default ("b")))
        ("@{a|{b}}"  ((:ref/list ("a") :default ("{b")) "}"))
        ("@{a|${b}}" (:ref/list ("a") :default ((:ref ("b")))))
        ("@{@{a}}"   (:ref/list ((:ref/list ("a")))))))

(declaim (special *seen*))

(defvar *seen* nil
  "TODO(jmoringe): document")

(defun expand (pattern lookup)
  (let+ (((&flet lookup (name)
            (let+ (((&values value parsed?)
                    (funcall lookup (make-keyword (string-upcase name)))))
              (parse value :parse-strings? (not parsed?)))))
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
               (recur (or (ignore-errors (lookup pattern)) default)))

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

(defmethod value ((thing t) (name t))
  (let+ (((&values raw raw/next-values) (lookup thing name))
         ((&labels+ make-lookup ((&optional first-value &rest next-values))
            (lambda (name1)
              (cond
                ((not (eq name1 :next-value))
                 (values (value thing name1) t))
                (first-value
                 (values (expand (parse first-value)
                                 (make-lookup next-values))
                         t))
                (t
                 (error "~@<No next value for ~A.~@:>"
                        name)))))))
    (expand (parse raw) (make-lookup raw/next-values))))

;;; Platform requirements protocol

(defgeneric platform-requires (object platform)
  (:documentation
   "Return a list of \"platform requirements\" for OBJECT and PLATFORM
    with elements of the form

      (NAME VERSION)"))

(defmethod platform-requires ((object t) (platform cons))
  (let+ ((spec (ignore-errors (value object :platform-requires)))
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
  (switch ((ignore-errors (value object :access)) :test #'equal)
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
                  (format stream "~@<Skip deployment of ~A.~@:>"
                          thing))
        (declare (ignore condition))
        nil))))
