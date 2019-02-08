;;;; protocol.lisp --- Protocol provided by the project module.
;;;;
;;;; Copyright (C) 2012-2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.model)

;;; Ancestors protocol

(defgeneric parent (thing)
  (:documentation
   "Return the parent of THING or NIL."))

(defgeneric ancestors (thing)
  (:documentation
   "Return a list of ancestors of THING, including THING."))

;;; Default behavior

(defmethod ancestors ((thing t))
  (list* thing
         (when-let ((parent (and (compute-applicable-methods
                                  #'parent (list thing))
                                 (parent thing))))
           (ancestors parent))))

;;; Named and ancestors protocol

(defgeneric ancestor-names (thing)
  (:documentation
   "Return a list of the names of the ancestors of THING.

    The firsts element of the returned list is the name of THING, the
    second element (if any) is the name of the parent of THING,
    etc."))

;;; Default behavior

(defmethod ancestor-names ((thing t))
  (list* (let ((name (name thing)))
           (if (emptyp name)
               "<empty>"
               name))
         (when-let ((parent (and (compute-applicable-methods
                                  #'parent (list thing))
                                 (parent thing))))
           (ancestor-names parent))))

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
  (value/cast object :access :public))

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
        (unless (eq (specification implementation) spec)
          (setf (%specification implementation) spec))
        (push implementation (implementations spec))
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

;;; Deployment protocol

(defgeneric deploy (thing)
  (:documentation
   "Deploy THING.

    Signal `deployment-condition's such as `deployment-error' when
    conditions such as errors are encountered."))

(defgeneric deploy-dependencies (thing)
  (:documentation
   "Deploy dependencies of THING.

    Signal `deployment-condition's such as `deployment-error' when
    conditions such as errors are encountered."))

;; Default behavior

(defmethod deploy :around ((thing t))
  (with-condition-translation (((error deployment-error)
                                :thing thing))
    (with-simple-restart (continue "~@<Skip deployment of ~A.~@:>" thing)
      (call-next-method))))

(defmethod deploy-dependencies :around ((thing t))
  (with-condition-translation (((error deployment-error)
                                :thing thing))
    (with-simple-restart (continue "~@<Skip deploying dependencies of ~
                                    ~A.~@:>"
                                   thing)
      (call-next-method))))

;;; Implementation protocol

(defgeneric specification (implementation)
  (:documentation
   "Return the specification according to which IMPLEMENTATION has
    been created."))

;;; Specification protocol

(defgeneric implementation (specification)
  (:documentation
   "Return the implementation that has been created according to
    SPECIFICATION.

    Asserts that there is exactly one implementation of
    SPECIFICATION."))

(defgeneric implementations (specification)
  (:documentation
   "Return all implementations that have been created according to
    SPECIFICATION."))
