;;;; recipe-repository.lisp --- A repository for directory and filename information.
;;;;
;;;; Copyright (C) 2019, 2020, 2022 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:build-generator.model.project)

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
  `((:name "~A" ,(name object))))

(defun ensure-mode (thing)
  (labels ((rec (thing)
             (etypecase thing
               (mode   thing)
               (string (make-instance 'mode :name thing))
               (cons   (let+ (((first &rest rest) thing))
                         (make-instance 'mode :name   first
                                              :parent (when rest
                                                        (rec rest))))))))
    (rec thing)))

;;; `recipe-repository'

(defclass recipe-repository (print-items:print-items-mixin)
  ((%name               :initarg  :name
                        :reader   name
                        :writer   (setf %name))
   (%root-directory     :initarg  :root-directory
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
                         template recipes.")
   (%parents            :initarg  :parents
                        :type     list
                        :reader   parents
                        :initform '()
                        :documentation
                        "A list of parent repositories that should be
                         searched for recipes not found in this one."))
  (:default-initargs
   :root-directory (missing-required-initarg 'recipe-repository :root-directory)
   :mode           (missing-required-initarg 'recipe-repository :mode))
  (:documentation
   "Stores a repository root and sub-directories for recipe kinds."))

(defmethod shared-initialize :after ((instance   recipe-repository)
                                     (slot-names t)
                                     &key
                                     (name           nil name-supplied?)
                                     (root-directory nil root-directory-supplied?))
  (declare (ignore name))
  (when (and (not name-supplied?) root-directory-supplied?)
    (setf (%name instance) (lastcar (pathname-directory root-directory)))))

(defmethod print-items:print-items append ((object recipe-repository))
  `((:name                       "~A"       ,(name object))
    ((:mode      (:after :name)) " ~A mode" ,(name (mode object)))
    ((:root-path (:after :mode)) " [~A]"    ,(root-directory object))))

(defmethod describe-object ((object recipe-repository) stream)
  (utilities.print-tree:print-tree
   stream object
   (utilities.print-tree:make-node-printer
    (lambda (stream depth node)
      (declare (ignore depth))
      (print-items:format-print-items stream (print-items:print-items node))
      nil)
    nil #'parents)))

(defun make-recipe-repository (root-directory mode-or-modes
                               &rest args &key name parents)
  (declare (ignore name parents))
  (let ((root-directory (truename root-directory))
        (mode           (ensure-mode mode-or-modes)))
    (apply #'make-instance 'recipe-repository :root-directory root-directory
                                              :mode           mode
                                              args)))

(defun populate-recipe-repository! (repository)
  (setf (recipe-directory :template     repository) #P"templates/"
        (recipe-directory :project      repository) #P"projects/"
        (recipe-directory :distribution repository) #P"distributions/"
        (recipe-directory :person       repository) #P"persons/")
  repository)

(defun make-populated-recipe-repository (root-directory mode-or-modes
                                         &rest args &key name parents)
  (declare (ignore name parents))
  (populate-recipe-repository!
   (apply #'make-recipe-repository root-directory mode-or-modes args)))

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

;;; Name -> pathname

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
                        (name       (eql :wild-inferiors)))
  (let ((name (make-pathname :name      :wild
                             :directory `(:relative :wild-inferiors))))
    (recipe-path repository kind name)))

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

;;; `probe-recipe-pathname'

(defmethod probe-recipe-pathname ((repository recipe-repository)
                                  (pathname   pathname))
  (log:info "~@<Looking for ~A in ~A~@:>" pathname repository)
  (cond ((wild-pathname-p pathname)
         (directory pathname))
        ((probe-file pathname)
         (list pathname))
        (t
         nil)))

;;; `recipe-truename'

(defmethod recipe-truename :around ((repository recipe-repository)
                                    (kind       t)
                                    (name       t)
                                    &key
                                    (recursive?        t)
                                    (if-does-not-exist #'error))
  (or (call-next-method repository kind name :if-does-not-exist nil)
      (when recursive?
        (some (rcurry #'recipe-truename kind name
                      :if-does-not-exist nil)
              (parents repository)))
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
    (when (probe-recipe-pathname repository pathname)
      pathname)))

(defmethod recipe-truename ((repository recipe-repository)
                            (kind       (eql :template))
                            (name       t)
                            &key if-does-not-exist)
  (declare (ignore if-does-not-exist))
  (recipe-truename repository (mode repository) name
                   :recursive? nil :if-does-not-exist nil))

(defmethod recipe-truename ((repository recipe-repository)
                            (kind       mode)
                            (name       t)
                            &key if-does-not-exist)
  (declare (ignore if-does-not-exist))
  (or (call-next-method repository kind name :if-does-not-exist nil)
      (when-let ((parent (parent kind)))
        (recipe-truename repository parent name
                         :recursive? nil :if-does-not-exist nil))))

;;; `recipe-truenames'

(defmethod recipe-truenames :around ((repository t) (kind t) (name t)
                                     &key (recursive? t))
  (let ((local-results (call-next-method)))
    (if recursive?
        (append local-results
                (mappend (rcurry #'recipe-truenames kind name)
                         (parents repository)))
        local-results)))

(defmethod recipe-truenames ((repository recipe-repository)
                             (kind       t)
                             (name       t)
                             &key)
  (probe-recipe-pathname repository (recipe-path repository kind name)))

(defmethod recipe-truenames ((repository recipe-repository)
                             (kind       (eql :template))
                             (name       t)
                             &key)
  (recipe-truenames repository (mode repository) name :recursive? nil))

(defmethod recipe-truenames ((repository recipe-repository)
                             (kind       mode)
                             (name       t)
                             &key)
  (append (call-next-method)
          (when-let ((parent (parent kind)))
            (recipe-truenames repository parent name :recursive? nil))))

;;; Pathname -> name

(defmethod recipe-name :around ((repository recipe-repository)
                                (kind       t)
                                (path       pathname)
                                &key (recursive? t))
  (multiple-value-bind (name container) (call-next-method)
    (cond (name
           (values name container))
          (recursive?
           (map nil (lambda (parent)
                      (multiple-value-bind (name container)
                          (recipe-name parent kind path)
                        (when name
                          (return-from recipe-name (values name container)))))
                (parents repository))))))

(defmethod recipe-name ((repository recipe-repository)
                        (kind       t)
                        (path       pathname)
                        &key)
  (assert (uiop:absolute-pathname-p path))
  (let ((directory    (recipe-directory kind repository))
        (without-type (make-pathname :type nil :defaults path)))
    (when (uiop:subpathp without-type directory)
      (let ((namestring (uiop:native-namestring
                         (uiop:enough-pathname without-type directory))))
        (values namestring repository)))))

(defmethod recipe-name ((repository recipe-repository)
                        (kind       (eql :template))
                        (path       pathname)
                        &key)
  (recipe-name repository (mode repository) path :recursive? nil))

(defmethod recipe-name ((repository recipe-repository)
                        (kind       mode)
                        (path       pathname)
                        &key)
  (multiple-value-bind (name container) (call-next-method)
    (if name
        (values name container)
        (when-let ((parent (parent kind)))
          (recipe-name repository parent path :recursive? nil)))))

;;; Parents files

(defun make-parents-filename (directory)
  (make-pathname :name "parents" :defaults directory))

(defun parse-parents-file (file)
  (let ((document (%load-yaml file)))
    (unless (typep document 'cons)
      (object-error
       (list (list document "non-sequence node" :error))
       "~@<parents file must contain a non-empty sequence at the ~
        top-level.~@:>"))
    (map 'list (lambda (element)
                 (unless (stringp element)
                   (object-error (list (list element "here" :error))
                                 "~@<Sequence element is not a string.~@:>"))
                 element)
         document)))

;;; Mode parent loading

(defun load-mode-parents-file (root-directory mode)
  (check-type mode string "A mode string")
  (let ((filename (make-parents-filename
                   ;; This computes the templates/MODE sub-directory
                   ;; "manually" since the repository is not yet
                   ;; available.
                   (merge-pathnames
                    (make-pathname :directory `(:relative "templates" ,mode))
                    root-directory))))
    (ensure-mode (list* mode (if (probe-file filename)
                                 (parse-parents-file filename)
                                 '("_common"))))))

;;; Repository parent loading

(defun load-repository-parents-file (root-directory)
  (let ((filename (make-parents-filename root-directory)))
    (when-let ((entries (when (probe-file filename)
                          (parse-parents-file filename))))
      (map 'list (lambda (entry)
                   (cond ((ppcre:scan "[^:]+://" entry)
                          (puri:uri entry))
                         (t
                          (uiop:ensure-directory-pathname entry))))
           entries))))

;;; Loading repositories

(defun load-parent-repositories (references mode &key cache-directory)
  (mapcan (lambda (reference)
            (with-simple-restart (continue "~@<Skip the recipe repository designated by ~A.~@:>" reference)
              (list (load-repository reference mode
                                     :cache-directory cache-directory))))
          references))

(defvar *loading-repositories?* nil)

(defmethod load-repository :around ((source t) (mode t) &key name
                                                             cache-directory)
  (declare (ignore name cache-directory))
  (if *loading-repositories?*
      (call-next-method)
      (let ((*loading-repositories?* t))
        (with-trivial-progress (:load-repository)
          (analysis::with-git-cache ()
            (let ((result (call-next-method)))
              (log:info "~@<~@;Loaded repository stack~@:_~A~@:>"
                        (with-output-to-string (stream)
                          (describe result stream)))
              result))))))

(defmethod load-repository ((source pathname) (mode t) &key name
                                                            cache-directory)
  (progress :load-repository nil "~A" source)
  (unless (uiop:directory-pathname-p source)
    (error "~@<Repository pathname ~A does not designate a directory.~@:>"
           source))
  (let* ((source-truename   (or (probe-file source)
                                (error "~@<Repository directory ~A ~
                                        does not exist.~@:>"
                                       source)))
         (mode              (load-mode-parents-file source mode))
         (parent-references (load-repository-parents-file source-truename))
         (parents           (let ((*default-pathname-defaults* source-truename))
                              (load-parent-repositories
                               parent-references (name mode)
                               :cache-directory cache-directory))))
    (apply #'make-populated-recipe-repository source-truename mode
           :parents parents (when name (list :name name)))))

(defmethod load-repository ((source puri:uri) (mode t) &key name
                                                            cache-directory)
  (unless cache-directory
    (error "~@<Cannot load remote repository ~A without cache ~
            directory.~@:>"
         source))
  (let* ((branch          (puri:uri-fragment source))
         (name            (or name
                              (format nil "~A~@[@~A~]"
                                      (lastcar (puri:uri-parsed-path source))
                                      branch)))
         (clone-directory (merge-pathnames
                           (make-pathname :directory (list :relative name))
                           cache-directory)))
    (uiop:delete-directory-tree
     clone-directory :validate t :if-does-not-exist :ignore)
    (more-conditions::without-progress
      (apply #'analysis::clone-git-repository/maybe-cached
             source clone-directory
             :cache-directory cache-directory
             :history-limit   1
             (when branch (list :branch branch))))
    (load-repository clone-directory mode
                     :name            name
                     :cache-directory cache-directory)))
