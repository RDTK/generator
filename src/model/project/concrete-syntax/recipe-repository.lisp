;;;; recipe-repository.lisp --- A repository for directory and filename information.
;;;;
;;;; Copyright (C) 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.model.project)

;;; `mode'

(defclass mode (print-items:print-items-mixin)
  ((%name   :initarg  :name
            :type     string
            :reader   name)
   (%parent :initarg  :parent
            :reader   parent
            :initform nil))
  (:default-initargs
   :name (missing-required-initarg 'mode :name))
  (:documentation
   "Represents a processing mode, ultimately selecting template recipes."))

(defmethod print-items:print-items append ((object mode))
  `((:name ,(name object) "~A")))

(defun ensure-mode (thing)
  (labels ((rec (thing)
             (etypecase thing
               (mode   thing)
               (string (make-instance 'mode :name thing))
               (cons   (let+ (((first &optional rest) thing))
                         (make-instance 'mode :name   first
                                              :parent (when rest
                                                        (rec rest))))))))
    (rec thing)))

;;; `recipe-repository'

(defclass recipe-repository (print-items:print-items-mixin)
  ((%root-directory     :initarg  :root-directory
                        :type     (and pathname (satisfies uiop:directory-pathname-p))
                        :reader   root-directory
                        :documentation
                        "The root directory of the repository.")
   (%recipe-directories :type     hash-table
                        :reader   %recipe-directories
                        :initform (make-hash-table :test #'eq)
                        :documentation
                        "A map of recipe kinds to sub-directories.")
   (%mode               :initarg  :mode
                        :reader   mode
                        :documentation
                        "The mode this repository should use to locate
                         template recipes."))
  (:default-initargs
   :root-directory (missing-required-initarg 'recipe-repository :root-directory)
   :mode           (missing-required-initarg 'recipe-repository :mode))
  (:documentation
   "Stores a repository root and sub-directories for recipe kinds."))

(defmethod print-items:print-items append ((object recipe-repository))
  `((:root-path ,(root-directory object) "~A")
    (:mode      ,(name (mode object))    " ~A mode" ((:after :root-path)))))

(defun make-recipe-repository (root-directory mode &rest parent-modes)
  (let ((mode (ensure-mode (list* mode parent-modes))))
    (make-instance 'recipe-repository :root-directory root-directory
                                      :mode           mode)))

(defun populate-recipe-repository! (repository)
  (setf (recipe-directory :template     repository) #P"templates/"
        (recipe-directory :project      repository) #P"projects/"
        (recipe-directory :distribution repository) #P"distributions/"
        (recipe-directory :person       repository) #P"persons/")
  repository)

(defun make-populated-recipe-repository (root-directory mode &rest parent-modes)
  (populate-recipe-repository!
   (apply #'make-recipe-repository root-directory mode parent-modes)))

(defmethod recipe-directory ((kind t) (repository recipe-repository))
  (let ((relative (or (gethash kind (%recipe-directories repository))
                      (error "~@<~A does not have a sub-directory for ~
                              recipes of kind ~A.~@:>"
                             repository kind))))
    (merge-pathnames relative (root-directory repository))))

(defmethod recipe-directory ((kind mode) (repository recipe-repository))
  (merge-pathnames (make-pathname :directory `(:relative ,(name kind)))
                   (recipe-directory :template repository)))

(defmethod (setf recipe-directory) ((new-value  pathname)
                                    (kind       t)
                                    (repository recipe-repository))
  (unless (uiop:directory-pathname-p new-value)
    (error "~@<~A is not a directory pathname.~@:>" new-value))

  (setf (gethash kind (%recipe-directories repository)) new-value))

;;; `recipe-path'

(defmethod recipe-path ((repository recipe-repository)
                        (kind       t)
                        (name       string))
  (let* ((components (split-sequence:split-sequence #\/ name))
         (directory  (list* :relative (butlast components)))
         (name       (lastcar components))
         (pathname   (make-pathname :directory directory
                                    :name      name)))
    (recipe-path repository kind pathname)))

(defmethod recipe-path ((repository recipe-repository)
                        (kind       t)
                        (name       (eql :wild)))
  (recipe-path repository kind (make-pathname :name name)))

(defmethod recipe-path ((repository recipe-repository)
                        (kind       t)
                        (name       pathname))
  (let* ((directory  (recipe-directory kind repository))
         (type       (string-downcase kind))
         (defaults   (make-pathname :type type :defaults directory)))
    (merge-pathnames name defaults)))

(defmethod recipe-path ((repository recipe-repository)
                        (kind       (eql :template))
                        (name       t))
  (recipe-path repository (mode repository) name))

(defmethod recipe-path ((repository recipe-repository)
                        (kind       mode)
                        (name       pathname))
  (let* ((directory  (recipe-directory kind repository))
         (type       (string-downcase :template))
         (defaults   (make-pathname :type type :defaults directory)))
    (merge-pathnames name defaults)))

;;; `recipe-truename'

(defmethod recipe-truename :around ((repository recipe-repository)
                                    (kind       t)
                                    (name       t)
                                    &key
                                    (if-does-not-exist #'error))
  (or (call-next-method repository kind name :if-does-not-exist nil)
      (error-behavior-restart-case
          (if-does-not-exist (recipe-not-found-error
                              :kind       kind
                              :name       name
                              :repository repository)))))

(defmethod recipe-truename ((repository recipe-repository)
                            (kind       t)
                            (name       t)
                            &key if-does-not-exist)
  (declare (ignore if-does-not-exist))
  (let ((pathname (recipe-path repository kind name)))
    (cond ((and (wild-pathname-p pathname) (directory pathname))
           pathname)
          ((and (not (wild-pathname-p pathname)) (probe-file pathname))
           pathname))))

(defmethod recipe-truename ((repository recipe-repository)
                            (kind       (eql :template))
                            (name       t)
                            &key if-does-not-exist)
  (declare (ignore if-does-not-exist))
  (recipe-truename repository (mode repository) name :if-does-not-exist nil))

(defmethod recipe-truename ((repository recipe-repository)
                            (kind       mode)
                            (name       t)
                            &key if-does-not-exist)
  (declare (ignore if-does-not-exist))
  (or (call-next-method repository kind name :if-does-not-exist nil)
      (when-let ((parent (parent kind)))
        (recipe-truename repository parent name :if-does-not-exist nil))))

;;; `recipe-truenames'

(defmethod recipe-truenames ((repository recipe-repository)
                             (kind       t)
                             (name       t))
  (let ((pathname (recipe-path repository kind name)))
    (cond ((wild-pathname-p pathname)
           (directory pathname))
          ((probe-file pathname)
           (list pathname))
          (t
           '()))))

(defmethod recipe-truenames ((repository recipe-repository)
                             (kind       (eql :template))
                             (name       t))
  (recipe-truenames repository (mode repository) name))

(defmethod recipe-truenames ((repository recipe-repository)
                             (kind       mode)
                             (name       t))
  (append (call-next-method repository kind name)
          (when-let ((parent (parent kind)))
            (recipe-truenames repository parent name))))
