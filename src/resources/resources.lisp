;;;; resources.lisp --- Operations and classes of the resources module.
;;;;
;;;; Copyright (C) 2019, 2022 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:build-generator.resources)

;;; `entry'

(defclass entry (print-items:print-items-mixin)
  ((%name    :initarg  :name
             :type     pathname
             :reader   name)
   (%content :initarg  :content
             :type     nibbles:octet-vector
             :reader   content)
   (%info    :initarg  :info
             :type     list
             :reader   info
             :initform '()))
  (:default-initargs
   :name    (missing-required-initarg 'entry :name)
   :content (missing-required-initarg 'entry :content)))

(defmethod print-items:print-items append ((object entry))
  `((:name                                "~A"            ,(name object))
    ((:octet-count (:after :name))        " ~:D octet~:P" ,(length (content object)))
    ((:info        (:after :octet-count)) "~@[ ~S~]"      ,(info object))))

;;; `group'

(defclass group (print-items:print-items-mixin)
  ((%name    :initarg  :name
             :reader   name)
   (%parent  :initarg  :parent
             :reader   parent)
   (%entries :reader   %entries
             :initform (make-hash-table :test #'equalp)))
  (:default-initargs
   :name   (missing-required-initarg 'group :name)
   :parent (missing-required-initarg 'group :parent)))

(defmethod initialize-instance :after ((instance group)
                                       &key
                                       name
                                       parent)
  (setf (find-group name parent) instance))

(defmethod print-items:print-items append ((object group))
  (let ((name  (name object))
        (count (hash-table-count (%entries object))))
    `((:name                         "~A"            ,name)
      ((:entry-count (:after :name)) " ~:D entr~:@P" ,count))))

(defmethod entries ((container group))
  (hash-table-values (%entries container)))

(defmethod find-entry ((name pathname) (container group)
                       &key (if-does-not-exist #'error))
  (or (gethash name (%entries container))
      (error-behavior-restart-case
          (if-does-not-exist
           (entry-does-not-exist-error :name name :group container)))))

(defmethod (setf find-entry) ((new-value t)
                              (name      pathname)
                              (container group)
                              &key if-does-not-exist)
  (declare (ignore if-does-not-exist))
  (log:debug "~@<~A is adding entry ~A~@:>" container new-value)
  (setf (gethash name (%entries container)) new-value))

(defmethod add-file ((container group) (file vector)
                     &key
                     base-directory
                     (name (missing-required-argument :name))
                     info)
  (declare (ignore base-directory))
  (check-type file nibbles:octet-vector)
  (let* ((content (ensure-datum file (parent container)))
         (entry   (make-instance 'entry :name    name
                                        :content content
                                        :info    info)))
    (setf (find-entry name container) entry)
    (values container entry)))

(defmethod add-file ((container group) (file pathname)
                     &key
                     (base-directory *default-pathname-defaults*)
                     (name           (uiop:enough-pathname file base-directory))
                     info)
  (cond ((wild-pathname-p file)
         (map nil (lambda (file)
                    (when (uiop:file-pathname-p file)
                      (add-file container file :base-directory base-directory
                                               :info           info)))
              (directory file :resolve-symlinks nil)))
        ((uiop:directory-pathname-p file)
         (let ((pattern (merge-pathnames "**/*.*" file)))
           (add-file container pattern :base-directory file
                                       :info           info)))
        (t
         (let ((content (read-file-into-byte-vector file))
               #+sbcl (mode (logand #o777 (sb-posix:stat-mode
                                           (sb-posix:stat file)))))
           (add-file container content :name name
                                       :info (list* :mode mode info))))))

;;; `resources'

(defclass resources (print-items:print-items-mixin)
  ((%groups :reader   groups
            :initform (make-hash-table :test #'eq))
   (%data   :reader   data
            :initform (make-hash-table :test     #'equalp
                                       :weakness :key-and-value))))

(defmethod print-items:print-items append ((object resources))
  (let ((group-count (hash-table-count (groups object)))
        (datum-count (hash-table-count (data object)))
        (octet-count (octet-count object)))
   `((:group-count                         "~:D group~:P"  ,group-count)
     ((:datum-count (:after :group-count)) " ~:D datum~:P" ,datum-count)
     ((:octet-count (:after :datum-count)) " ~:D octet~:P" ,octet-count))))

(defmethod octet-count ((container resources))
  (reduce #'+ (hash-table-keys (data container)) :key #'length))

(defmethod find-group ((name symbol) (container resources)
                       &key (if-does-not-exist #'error))
  (or (gethash name (groups container))
      (error-behavior-restart-case
          (if-does-not-exist
           (group-does-not-exist-error :name name :resources container)))))

(defmethod (setf find-group) ((new-value t)
                              (name      symbol)
                              (container resources)
                              &key if-does-not-exist)
  (declare (ignore if-does-not-exist))
  (log:debug "~@<~A is adding ~A~@:>" container new-value)
  (setf (gethash name (groups container)) new-value))

(defmethod ensure-datum ((datum vector) (container resources))
  (ensure-gethash datum (data container) datum))

;;; Global resource group registry

(defvar *resources* (make-instance 'resources))

(defun make-group (name &key (parent *resources*))
  (make-instance 'group :name name :parent parent))

(defun find-group* (name &key (parent *resources*) (if-does-not-exist #'error))
  (find-group name parent :if-does-not-exist if-does-not-exist))
