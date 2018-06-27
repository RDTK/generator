;;;; interface.lisp --- Implementations of Jenkins interfaces.
;;;;
;;;; Copyright (C) 2012-2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.api)

(defmacro define-interface-implementations ((name
                                             &key
                                             (class-location '(xloc:name ".")))
                                            &body implementations)
  (let+ (((class-accessor class-path) class-location)
         (name->class-table (format-symbol *package* "*NAME->~A-CLASS*" name))
         (class->name-table (format-symbol *package* "*CLASS->~A-NAME*" name))
         ((&flet+ make-implementation (((key class &key plugin) (&rest slots) &body options))
            (let ((class-name (format-symbol *package* "~A/~A" name key)))
              `((setf (gethash ,class       ,name->class-table) ',class-name
                      (gethash ',class-name ,class->name-table) ,class)

                (define-model-class ,class-name ()
                  (,@(when plugin
                       `((%plugin :type     string
                                  :xpath    "@plugin"
                                  :initform ,plugin)))
                   ,@slots)
                  ,@options
                  (:version-slot %plugin)))))))
    `(progn
       (deftype ,name () t)

       (defvar ,name->class-table (make-hash-table :test #'equal))
       (defvar ,class->name-table (make-hash-table :test #'eq))

       (defmethod xloc:xml-> ((value stp:element)
                              (type  (eql ',name))
                              &key &allow-other-keys)
         ;; Try to look up the implementation class for the
         ;; implementation name stored in VALUE. If the class cannot be
         ;; found, signal an `unmapped-class' condition and return a
         ;; marker object.
         (let ((name (,class-accessor (xloc:loc value ,class-path))))
           (if-let ((class-name (gethash name ,name->class-table)))
             (xloc:xml-> value class-name)
             (progn
               (signal 'unmapped-class
                       :interface ',name
                       :name      name)
               (list :unimplemented ',name name value)))))

       (defmethod xloc:->xml ((value t)
                              (dest  stp:element)
                              (type  (eql ',name))
                              &key &allow-other-keys)
         (let* ((class-name (class-name (class-of value)))
                (name       (gethash class-name ,class->name-table)))
           (unless name
             (error ,(format nil "~~@<~~A is not a valid ~S ~
                                 class. Valid ~:*~S classes are ~
                                 ~~{~~S~~^, ~~}.~~@:>"
                             name)
                    value (hash-table-keys ,class->name-table)))

           (setf (,class-accessor (xloc:loc dest ,class-path :if-no-match :create)) name)
           (xloc:->xml value dest class-name)))

       (defmethod xloc:->xml ((value list)
                              (dest  stp:element)
                              (type  (eql ',name))
                              &key &allow-other-keys)
         "This helper method ensures that XML substree DEST is still in
         sync with the XML substree stored in the unmapped
         implementation marker VALUE."
         (check-type value unmapped-marker)
         (assert (eq dest (fourth value))))

       ,@(mappend #'make-implementation implementations))))
