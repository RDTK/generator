;;;; model-class.lisp --- Superclass for model classes.
;;;;
;;;; Copyright (C) 2012-2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.api)

(defvar *initializing?* nil)

(defclass standard-model-object ()
  ((id       :initarg  :id
             :accessor id
             :documentation
             "")
   (data     :initarg  :data
             :accessor %data
             :documentation
             "")
   (get-func :initarg  :get-func
             :type     function
             :accessor get-func
             :documentation
             "")
   (put-func :initarg  :put-func
             :type     (or null function)
             :accessor put-func
             :initform nil
             :documentation
             ""))
  (:default-initargs
   :id       (missing-required-initarg 'standard-model-object :id)
   :get-func (missing-required-initarg 'standard-model-object :get-func))
  (:documentation
   "A superclass for model classes."))

(defmethod shared-initialize :around ((instance    standard-model-object)
                                      (slots-names t)
                                      &key)
  (let ((*initializing?* t))
    (call-next-method)))

(defmethod slot-unbound :around ((class     t)
                                 (instance  standard-model-object)
                                 (slot-name t))
  (setf (%data instance) nil)
  (update! instance)
  (if (slot-boundp instance slot-name)
      (slot-value instance slot-name)
      (call-next-method)))

(defmethod (setf closer-mop:slot-value-using-class) :around ((new-value t)
                                                             (class     t)
                                                             (instance  standard-model-object)
                                                             (slot      t))
  (when (and (not (member (closer-mop:slot-definition-name slot) '(id data get-func put-func)))
             (not (slot-boundp instance 'data))
             (not *initializing?*))
    (update! instance))
  (call-next-method))

;; TODO(jmoringe, 2013-02-21): rename root? -> toplevel? ? make option?
(defmacro define-model-class (name () (&rest slots) &body options)
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
              `(,initarg (missing-required-initarg ',name ,initarg)))))

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
         (get-func     (second (find :get-func options :key #'first)))
         (put-func     (second (find :put-func options :key #'first)))
         (root?        (or get-func put-func)))
    `(progn
       (defclass ,name (,@(when root? '(standard-model-object)))
         (,@(mapcar #'make-slot-spec slots))
         (:default-initargs
          ,@(append
             (when get-func `(:get-func ,get-func))
             (when put-func `(:put-func ,put-func))
             (mappend #'make-default-initarg slots)
             (rest (find :default-initargs options :key #'first))))
         ,@(remove-if (lambda (key)
                        (member key '(:name-slot :version-slot
                                      :get-func :put-func
                                      :default-initargs)))
                      options :key #'first))

       ;; TODO(jmoringe, 2013-01-03): get rid of this
       (defmethod ,name ((id t) &rest initargs &key &allow-other-keys)
         (apply #'make-instance ',name :id id initargs))

       (defmethod xloc:xml-> ((value stp:element)
                              (type  ,name)
                              &key &allow-other-keys)
         (flet (,@(when version-slot
                    `((version () (,version-slot type)))))
           ,@(when version-slot '((declare (ignorable #'version))))
           ,@(mapcar #'make-xml->slot slots))
         type)

       (defmethod xloc:->xml ((value ,name)
                              (dest  stp:element)
                              (type  (eql ',name))
                              &key &allow-other-keys)
         (flet (,@(when version-slot
                    `((version () (,version-slot value)))))
           ,@(when version-slot '((declare (ignorable #'version))))
           ,@(mapcar #'make-slot->xml slots))
         dest)

       ,@(when root?
           `((defmethod update! ((object ,name))
               (let+ (((&accessors id (data %data) get-func) object))
                 (setf data (funcall get-func id))
                 (xloc:xml-> (stp:root data) object)))

             (defmethod commit! ((object ,name))
               (let+ (((&accessors-r/o id (data %data) put-func) object))
                 (unless put-func
                   (error "~@<Read-only object ~A.~@:>" object))
                 (xloc:->xml object (stp:root data) ',name)
                 (funcall put-func id data))
               object)))

       ,@(when name-slot
           `((defmethod print-object ((object ,name) stream)
               (print-unreadable-object (object stream :type t :identity t)
                 (let* ((value (,name-slot object))
                        (end   (when (stringp value)
                                 (min (or (position #\Newline value)
                                          (length value))
                                      30))))
                   (princ (if end (subseq value 0 end) value) stream)))))))))
