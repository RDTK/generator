;;;; model-class.lisp --- Superclass for model classes.
;;;;
;;;; Copyright (C) 2012-2020 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.api)

;;; `standard-model-object'

(defclass standard-model-object ()
  ()
  (:documentation
   "A superclass for model object classes."))

;;; `root-model-object'

(defclass root-model-object (standard-model-object)
  ((id       :initarg  :id
             :accessor id
             :documentation
             "The unique id of this object.")
   (data     :initarg  :data
             :accessor %data
             :documentation
             "An XML document storing a serialized version of the data
              in this object.")
   (get-func :initarg  :get-func
             :type     (or null function)
             :accessor get-func
             :initform nil
             :documentation
             "A function that is called with one argument, the object
              id, and retrieves and returns the XML data for the
              specified object from the server.")
   (put-func :initarg  :put-func
             :type     (or null function)
             :accessor put-func
             :initform nil
             :documentation
             "A function that is called with two argument, an XML
              document containing the new data for the object and the
              object id, and stores the supplied data for specified
              object on the server."))
  (:default-initargs
   :id (missing-required-initarg 'standard-model-object :id))
  (:documentation
   "Class for model objects, that are roots.

    Root objects store an XML document in the non-root elements of
    which correspond to non-root model objects.

    Root objects can also store \"get\" and \"put\" functions which
    download and upload this XML configuration from/to Jenkins
    respectively."))

(defvar *initializing?* nil)

(defmethod shared-initialize :around ((instance    root-model-object)
                                      (slots-names t)
                                      &key)
  (let ((*initializing?* t))
    (call-next-method)))

(defmethod slot-unbound :around ((class     t)
                                 (instance  root-model-object)
                                 (slot-name t))
  (setf (%data instance) nil)
  (update! instance)
  (if (slot-boundp instance slot-name)
      (slot-value instance slot-name)
      (call-next-method)))

(defmethod (setf closer-mop:slot-value-using-class)
    :around ((new-value t)
             (class     t)
             (instance  root-model-object)
             (slot      t))
  (when (and (not (member (c2mop:slot-definition-name slot)
                          '(id data get-func put-func)))
             (not (slot-boundp instance 'data))
             (not *initializing?*))
    (update! instance))
  (call-next-method))

(defmethod update! ((object root-model-object))
  (let+ (((&accessors id (data %data) get-func) object))
    (setf data (funcall get-func id))
    (xloc:xml-> (stp:root data) object)))

(defmethod commit! ((object root-model-object))
  (let+ (((&accessors-r/o id (data %data) put-func) object))
    (unless put-func
      (error "~@<Read-only object ~A.~@:>" object))
    (xloc:->xml object (stp:root data) (class-name (class-of object)))
    (funcall put-func id data))
  object)

;;; `define-model-class' macro

(defmacro define-model-class (name (&rest superclasses) (&rest slots)
                              &body options)
  (let+ (((&flet maybe-version-case (spec &key (transform #'identity))
            (typecase spec
              ((cons (eql :version))
               `(cond
                  ,@(mapcar (lambda+ ((pattern expression))
                              (let ((body (funcall transform expression)))
                                (case pattern
                                  ((t) `(t                          ,body))
                                  (t   `((equal (version) ,pattern) ,body)))))
                            (rest spec))))
              (t (funcall transform spec)))))

         ((&flet+ make-slot-spec ((name
                                   &rest options
                                   &key
                                   (initarg   (make-keyword name))
                                   (type      t)
                                   xpath
                                   (optional? t)
                                   (accessor  name)
                                   &allow-other-keys))
            `(,name
              :initarg  ,initarg
              :type     ,(cond
                           ((and (not (typep xpath '(cons (eql :version))))
                                 (eq (getf (rest (ensure-list xpath)) :if-multiple-matches) :all))
                            'list)
                           ((and (listp type) (starts-with-subseq "LIST" (string (first type))))
                            'list)
                           (optional?
                            `(or null ,type))
                           (t
                            type))
              :accessor ,accessor
              ,@(remove-from-plist options :type :xpath :optional?))))

         ((&flet+ make-default-initarg ((slot-name
                                         &key
                                         (initarg       (make-keyword slot-name))
                                         (optional? t)
                                         (initform  nil initform-supplied?)
                                         &allow-other-keys))
            (declare (ignore initform))
            (unless (or optional? initform-supplied?)
              `(,initarg (missing-required-initarg ',name ,initarg))))) ; TODO class-name instead of name

         ((&flet+ make-xml->slot ((name
                                   &key
                                   (type      t)
                                   (xpath     (format nil "./~(~A~)/text()" name))
                                   (optional? t)
                                   &allow-other-keys))
            (when xpath
              `(let ((loc (apply #'xloc:loc value
                                 (append
                                  ,(maybe-version-case
                                    xpath :transform (lambda (x)
                                                       `(list ,@(ensure-list x))) )
                                  ,@(when optional?
                                      '('(:if-no-match :do-nothing)))))))
                 (setf (slot-value type ',name)
                       (xloc:val loc :type ',type))))))

         ((&flet+ make-slot->xml ((name
                                   &key
                                   (type      t)
                                   (xpath     (format nil "./~(~A~)/text()" name))
                                   (optional? t)
                                   &allow-other-keys))
            (when xpath
              `(let ((loc (apply #'xloc:loc dest
                                 (append
                                  ,(maybe-version-case
                                    xpath :transform (lambda (x)
                                                       `(list ,@(ensure-list x))))
                                  (list :if-no-match (if (and ,optional? (not (slot-value value ',name)))
                                                         :do-nothing
                                                         :create))))))
                 (when (xloc:location-result loc)
                   (setf (xloc:val loc :type ',type)
                         (slot-value value ',name)))))))

         (name-slot    (second (or (find :name-slot options :key #'first)
                                   '(:name-slot id))))
         (version-slot (second (find :version-slot options :key #'first)))
         (root?        (second (find :root? options :key #'first))))
    `(progn
       (defclass ,name (,@superclasses
                        ,@(when root? '(root-model-object)))
         (,@(mapcar #'make-slot-spec slots))
         (:default-initargs
          ,@(append
             (mappend #'make-default-initarg slots)
             (rest (find :default-initargs options :key #'first))))
         ,@(remove-if (lambda (key)
                        (member key '(:name-slot :version-slot :root?
                                      :default-initargs)))
                      options :key #'first))

       (defmethod xloc:xml-> ((value stp:element)
                              (type  ,name)
                              &key &allow-other-keys)
         (when (next-method-p) (call-next-method))
         (flet (,@(when version-slot
                    `((version () (,version-slot type)))))
           ,@(when version-slot '((declare (ignorable #'version))))
           ,@(mapcar #'make-xml->slot slots))
         type)

       (defmethod xloc:->xml ((value ,name)
                              (dest  stp:element)
                              (type  (eql ',name))
                              &key &allow-other-keys)
         ,@(when superclasses
             (map 'list (lambda (superclass)
                          `(xloc:->xml value dest ',superclass))
                  superclasses))
         (flet (,@(when version-slot
                    `((version () (,version-slot value)))))
           ,@(when version-slot '((declare (ignorable #'version))))
           ,@(mapcar #'make-slot->xml slots))
         dest)

       ,@(when name-slot
           `((defmethod print-object ((object ,name) stream)
               (print-unreadable-object (object stream :type t :identity t)
                 (let* ((value (,name-slot object))
                        (end   (when (stringp value)
                                 (min (or (position #\Newline value)
                                          (length value))
                                      30))))
                   (princ (if end (subseq value 0 end) value) stream)))))))))
