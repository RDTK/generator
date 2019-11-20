;;;; context.lisp --- Context contributors for different recipe types.
;;;;
;;;; Copyright (C) 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:build-generator.language-server)

;;; Utilities

(defun structure-path (position document)
  (when-let* ((locations (lookup:lookup position (index document))))
    (let ((path (mappend
                 (lambda (node)
                   (when (typep node '(cons keyword))
                     (list (car node))))
                 (map 'list (lambda (location)
                              (project::object-at
                               location (locations document)))
                      locations))))
      ;; TODO removing duplicates should not be necessary
      (values (remove-duplicates path :test #'equal) locations))))

;;; `context'

(defclass context ()
  ((%location :initarg :location
              :reader  location)
   (%word     :initarg :word
              :accessor %word
              :initform nil))
  (:default-initargs
   :location (more-conditions:missing-required-initarg 'context :location)))

(defmethod word ((object context))
  (or (%word object)
      (setf (%word object) (sloc:content (location object)))))

;; TODO print-items for context

;;; `prefix-mixin'

(defclass prefix-mixin ()
  ((%prefix :initarg :prefix
            :type    string
            :reader  prefix)))

(defmethod print-items:print-items append ((object prefix-mixin))
  `((:prefix ,(prefix object) "~S")))

;;; `structure-context'

(defclass structure-context (context
                             print-items:print-items-mixin)
  ((%path :initarg :path
          :reader  path)))

(defmethod print-items:print-items append ((object structure-context))
  `((:path ,(reverse (path object)) "~{~A~^ » ~}")))

(defclass structure-context-contributor () ())

(defmethod contrib:context-contributions
    ((workspace   t)
     (document    t)
     (position    t)
     (contributor structure-context-contributor))
  (when-let* ((path (structure-path position document)))
    (list (make-instance 'structure-context
                         :location (first (lookup:lookup position (index document))) ; TODO repeated work in `structure-path'
                         :path     path))))

;;; Template name context

(defclass template-name-context (context
                                 prefix-mixin
                                 print-items:print-items-mixin)
  ())

(defclass template-name-context-contributor () ())

#+old (defmethod contrib:context-contributions
    ((workspace   t)
     (document    project-document)
     (position    t)
     (contributor template-name-context-contributor))
  (when-let* ((path  (structure-path position document)) ; TODO produces location
              (depth (position :templates path)))
    (when (eql depth 0)
      (let ((location (first (lookup:lookup position (index document)))))
        (list (make-instance 'template-name-context :location location))))))

(defmethod contrib:context-contributions
    ((workspace   t)
     (document    project-document)
     (position    t)
     (contributor template-name-context-contributor))
  (let+ (((&values path (&optional location &rest &ign))
          (structure-path position document))
         (thing (project::object-at location (locations document))))
    (when (and (ends-with-subseq '(:templates) path)
               (stringp thing)) ; HACK
      (list (make-instance 'template-name-context
                           :location location
                           :prefix   thing)))))

(defmethod contrib:context-contributions
    ((workspace   t)
     (document    template-document)
     (position    t)
     (contributor template-name-context-contributor))
  (when-let* ((path  (structure-path position document))
              (depth (position :inherit path)))
    (when (eql depth 0)
      (list (make-instance 'template-name-context
                           :location (first (lookup:lookup position (index document))))))))

;;; Variable context

(defclass variable-context (context)
  ((%container :initarg :container
               :reader  container)))

;; TODO make a variant that returns the whole hierarchy
(defun find-variable-container (locations document)
  (let ((document-locations (locations document)))
    (map nil (lambda (location)
               (let ((object (project::object-at location document-locations)))
                 (when (typep object 'var:direct-variables-mixin)
                   (return-from find-variable-container object))))
         locations)))

;;; Variable name context

(defclass variable-name-context (variable-context
                                 prefix-mixin
                                 print-items:print-items-mixin)
  ((%prefix-range :initarg :prefix-range
                  :reader  prefix-range)))

(defclass variable-reference-context (variable-name-context)
  ((%kind          :initarg :kind
                   :reader  kind)
   (%variable-name :initarg :variable-name
                   :reader  variable-name)
   (%definition    :initarg :definition
                   :reader  definition)))

(defclass variable-name-context-contributor ()
  ())

(defmethod contrib:context-contributions
    ((workspace    t)
     (document     t)
     (position     t)
     (contriubutor variable-name-context-contributor))
  (when-let* ((locations (lookup:lookup position (index document)))
              (location  (first locations))
              (leaf      (project::object-at location (locations document)))
              (leaf      (when (stringp leaf)
                           leaf))
              (path      (structure-path position document))
              (position* (position :variables path)))

    (cond ;; NAME: VALUE
          ;;   ^
          ((and (= position* 1)
                (string-equal leaf (first path))) ;; TODO could be deeper within dictionary value
           (list (make-instance 'variable-name-context
                                :location     location
                                :prefix       (string leaf)
                                :prefix-range (sloc:range location))))

          ;; NAME: … @{NAME} …
          ;; NAME: … ${NAME} …
          ;;           ^
          ((stringp leaf)
           (when-let* ((text     (sloc:content location))
                       (base     (sloc:index (sloc:start location)))
                       (relative (- (sloc:index position) base))
                       (limit    (min (+ relative 3) (length text)))
                       (start    (let ((start$ (search "${" text :end2 limit :from-end t))
                                       (start@ (search "@{" text :end2 limit :from-end t)))
                                   (cond ((and start$ start@) (max start$ start@))
                                         (start$)
                                         (start@))))
                       (end      (if-let ((end (search "}" text :start2 start)))
                                   (1+ end)
                                   (length text))))
             (when (<= relative end)
               (let* ((name       (subseq text (+ start 2) (1- end)))
                      (definition (when-let ((object (object document)))
                                    (var:lookup object (make-keyword (string-upcase name))
                                                :if-undefined nil))))
                 (list (make-instance 'variable-reference-context
                                      :location      location
                                      :prefix        text
                                      :prefix-range  (sloc:make-range
                                                      (+ base start) (+ base end)
                                                      (lsp:text document))
                                      :kind          (case (aref text start)
                                                       (#\$ :scalar) ; TODO store this while searching
                                                       (#\@ :list))
                                      :variable-name name
                                      :definition    definition)))))))))

;;; Variable value context

(defclass variable-value-context (variable-context)
  ((%variable-location :initarg :variable-location
                       :reader  variable-location)

   #+unused? (%prefix-range      :initarg :prefix-range
                       :reader  prefix-range)))

(defclass unknown-variable-value-context (variable-value-context)
  ((%variable-name :initarg :variable-name
                   :reader  variable-name)))

(defclass known-variable-value-context (variable-value-context)
  ((%variable-node :initarg :variable-node
                   :reader  variable-node)))

(defmethod variable-name ((context known-variable-value-context))
  (var:variable-info-name (variable-node context)))

(defclass variable-value-context-contributor () ())

(defmethod contrib:context-contributions
    ((workspace    t)
     (document     t)
     (position     t)
     (contriubutor variable-value-context-contributor))
  (let+ (((&values path (&whole locations &optional location &rest &ign))
          (structure-path position document))
         (container (find-variable-container locations document)))
    (when-let* ((position (position :variables path))
                (name     (when (plusp position)
                            (nth (1- position) path))))
      (list (if-let ((variable (var:find-variable name :if-does-not-exist nil)))
              (make-instance 'known-variable-value-context
                             :location          location
                             :container         container
                             :variable-location location ; TODO
                             :variable-node     variable
                                        ; :prefix-range
                             )
              (make-instance 'unknown-variable-value-context
                             :location          location
                             :container         container
                             :variable-location location
                             :variable-name     name))))))

;;; Project version reference context

(defclass project-name-context (context
                                prefix-mixin
                                print-items:print-items-mixin)
  ())

(defclass project-version-context (project-name-context) ; TODO correct superclass
  ((%project-name :initarg :project-name ; TODO look up the project
                  :reader  project-name)))

(defclass project-version-reference-context-contributor () ())

#+old (defmethod contrib:context-contributions
    ((workspace   t)
     (document    t)
     (position    t)
     (contributor project-version-reference-context-contributor))
  (let+ (((&values path locations) (structure-path position document))
         ((&values thing location)
          (loop :for location :in locations
                :for thing = (project::object-at location (locations document))
                :when (stringp thing)
                :return (values thing location)))
         (path-position (position :versions path))
         (location (first locations))) ; TODO
    (when (and (eql path-position 0) (stringp thing))
      (let (; (base (sloc:index (sloc:start )))
            (at   (position #\@ thing)))
        (log:error thing at)
        (cond ;; versions:
              ;; - PROJECT-NAME
              ;;     ^
              ((not at)
               (list (make-instance 'project-name-context
                                    :location location
                                    :prefix   thing)))

              ;; versions:
              ;; - PROJECT-NAME@…
              ;;     ^
              ((sloc:location< position (sloc::adjust (sloc:start location) :offset at))
               (list (make-instance 'project-name-context
                                    :location location
                                    :prefix   (string-trim " " (subseq thing 0 at)))))

              ;; versions:
              ;; - PROJECT-NAME@VERSION-NAME
              ;;                    ^
              (t
               (break)
               (list (make-instance 'project-version-context
                                    :location location
                                    :project-name (string-trim " " (subseq thing 0 at))
                                    :prefix       (string-trim " " (subseq thing (min (1+ at) (length thing))))))))))))

(defmethod contrib:context-contributions
    ((workspace   t)
     (document    t)
     (position    t)
     (contributor project-version-reference-context-contributor))
  (let+ (((&values path locations) (structure-path position document))
         ((&values include location)
          (loop :for location :in locations
                :for thing = (project::object-at location (locations document))
                :when (typep thing 'project::project-include)
                :return (values thing location)))
         (path-position (position :versions path))
         (location (first locations))) ; TODO

    (when (eql path-position 0)
      (cond ;; versions:
            ;; - PROJECT-NAME@VERSION-NAME
            ;;     ^
            ((and include
                  (project::project include)
                  (project::location-of (project::project include) (locations document))
                  (lookup:location-in? position (project::location-of (project::project include) (locations document))))
             (list (make-instance 'project-name-context
                                  :location (project::location-of (project::project include) (locations document))
                                  :prefix   (project::project include))))

            ;; versions:
            ;; - PROJECT-NAME@VERSION-NAME
            ;;                    ^
            ((and include
                  (project::version include)
                  (project::location-of (project::version include) (locations document))
                  (lookup:location-in? position (project::location-of (project::version include) (locations document))))
             (list (make-instance 'project-version-context
                                  :location location
                                  :project-name (project::project include)
                                  :prefix       (project::version include))))))))

;;; Distribution name context

(defclass distribution-name-context (context
                                     prefix-mixin
                                     print-items:print-items-mixin)
  ())

(defclass distribution-name-context-contributor () ())

(defmethod contrib:context-contributions
    ((workspace   t)
     (document    distribution-document)
     (position    t)
     (contributor distribution-name-context-contributor))
  (let+ (((&values path (&optional location &rest &ign))
          (structure-path position document))
         (thing (project::object-at location (locations document))))
    (when (and (stringp thing) (ends-with-subseq '(:include) path))
      (list (make-instance 'distribution-name-context
                           :location location
                           :prefix   thing)))))

;;; Aspect class context

(defclass aspect-class-context (context
                                prefix-mixin
                                print-items:print-items-mixin)
  ())

(defclass aspect-class-context-contributor () ())

(defmethod contrib:context-contributions
    ((workspace   t)
     (document    t)
     (position    t)
     (contributor aspect-class-context-contributor))
  (let+ (((&values path (&optional location &rest &ign))
          (structure-path position document))
         (thing (project::object-at location (locations document))))
    (when (ends-with-subseq '(:aspect :aspects) path)
      (list (make-instance 'aspect-class-context
                           :location location
                           :prefix   thing)))))

;;; System package name context

(defclass system-package-name-context (context)
  (#+no (%prefix :initarg :prefix
            :reader  prefix)))

(defclass system-package-name-context-contributor () ())

(defmethod contrib:context-contributions
    ((workspace   t)
     (document    t)
     (position    t)
     (contributor system-package-name-context-contributor))
  (let+ (((&values path (&optional location &rest &ign))
          (structure-path position document))
         (thing (project::object-at location (locations document))))
    (when (and (search '(:platform-requires :variables) path)
               (starts-with :packages path))
      (list (make-instance 'system-package-name-context
                           :location location
                           ; :prefix   thing
                           )))))
