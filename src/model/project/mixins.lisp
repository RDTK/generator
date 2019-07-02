;;;; mixins.lisp --- Mixin classes used in the model.project module.
;;;;
;;;; Copyright (C) 2017, 2018, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.model.project)

;;; `dependencies-mixin'

(defclass dependencies-mixin ()
  ((requires :initarg  :requires
             :type     list
             :reader   direct-requires
             :initform '()
             :documentation
             "A list of requirement descriptions. Elements are of the
              form

                (NATURE NAME [VERSION])

             .")
   (provides :initarg  :provides
             :type     list
             :reader   direct-provides
             :initform '()
             :documentation
             "A list of descriptions of provided things. Elements are
              of the form

                (NATURE NAME [VERSION])

              ."))
  (:documentation
   "Adds slots and accessors for required and provided features."))

(defmethod requires ((thing dependencies-mixin))
  (let ((dependencies (direct-requires thing)))
    (if (next-method-p)
        (append dependencies (call-next-method))
        dependencies)))

(defmethod provides ((thing dependencies-mixin))
  (let ((dependencies (direct-provides thing)))
    (if (next-method-p)
        (append dependencies (call-next-method))
        dependencies)))

;;; `dependencies-from-variables-mixin'

(defclass dependencies-from-variables-mixin ()
  ()
  (:documentation
   "Adds derivation of required and provided features from variables.

    Concretely, the values of the `:extra-requires' and
    `:extra-provides' variables are integrated into the return values
    of the `requires' and `provides' functions respectively."))

(defmethod requires ((thing dependencies-from-variables-mixin))
  (let ((dependencies (mapcar #'parse-dependency-spec
                              (var:value/cast thing :extra-requires '()))))
    (if (next-method-p)
        (append dependencies (call-next-method))
        dependencies)))

(defmethod provides ((thing dependencies-from-variables-mixin))
  (let ((dependencies (mapcar #'parse-dependency-spec
                              (var:value/cast thing :extra-provides '()))))
    (if (next-method-p)
        (append dependencies (call-next-method))
        dependencies)))

;;; `dependency-merging-mixin'

(defclass dependency-merging-mixin ()
  ()
  (:documentation
   "Adds merging of required and provided features.

    Intended to be used when then `requires' and `provides' functions
    may return redundant feature sets."))

(defmethod requires :around ((thing dependency-merging-mixin))
  (analysis:merge-dependencies (call-next-method)))

(defmethod provides :around ((thing dependency-merging-mixin))
  (analysis:merge-dependencies (call-next-method)))

;;; `person-container-mixin'

(defclass person-container-mixin ()
  ((persons :initarg  :persons
            :reader   persons-in-roles/plist
            :initform '()
            :documentation
            "Plist of the form

               (ROLE1 PERSON-LIST1 ...)"))
  (:documentation
   "Adds to classes a plist of roles and person lists."))

(defmethod persons ((container person-container-mixin))
  (loop :for (role persons) :on (persons-in-roles/plist container) :by #'cddr
     :appending persons))

(defmethod persons-in-role ((role symbol) (container person-container-mixin))
  (getf (persons-in-roles/plist container) role))
