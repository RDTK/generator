;;;; asdf.lisp --- Analysis of ASDF projects.
;;;;
;;;; Copyright (C) 2013, 2014, 2016, 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.analysis)

(defmethod analyze ((file pathname)
                    (kind (eql :asdf/one-file))
                    &key)
  (let+ (((&flet+ read-file-* ((which filename &key (at 0)))
            (funcall (ecase which
                       (:read-file-form 'uiop:safe-read-file-form)
                       (:read-file-line 'uiop:safe-read-file-line))
                     (uiop:subpathname file filename)
                     :at at :package :asdf-user)))
         ((&labels process-version (spec)
            (typecase spec
              ((cons (member :read-file-form :read-file-file))
               (process-version (read-file-* spec)))
              (string
               (parse-version spec)))))
         ((&labels dependency->list (dependency)
            (etypecase dependency
              ((cons (eql :require))
               (dependency->list (second dependency)))
              ((cons (eql :feature))
               (dependency->list (third dependency)))
              ((cons (eql :version))
               (list :asdf
                     (string-downcase (second dependency))
                     (process-version (third dependency))))
              ((or string symbol)
               (list :asdf (string-downcase dependency))))))
         ((&flet+ process-system-form ((&ign name
                                        &key
                                        version
                                        license
                                        author
                                        maintainer
                                        description
                                        defsystem-depends-on
                                        depends-on
                                        &allow-other-keys))
            `(:provides ((:asdf
                          ,(string-downcase name)
                          ,(process-version version)))
              :requires ,(mapcar #'dependency->list
                                 (append defsystem-depends-on
                                         depends-on))
              ,@(when description `(:description ,description))
              ,@(when author      `(:authors     ,(parse-people-list
                                                   author)))
              ,@(when maintainer  `(:maintainers ,(parse-people-list
                                                   maintainer)))
              ,@(when license     `(:license     ,license))))))
    (mapcar #'process-system-form (%extract-system-definition-forms file))))

(defmethod analyze ((directory pathname)
                    (kind      (eql :asdf))
                    &key)
  (let+ ((systems
          (iter (for file in (find-files (merge-pathnames "**/*.asd" directory)))
                (log:info "~@<Analyzing ~S.~@:>" file)
                (with-simple-restart (continue "~@<Skip file ~S.~@:>" file)
                  (appending (analyze file :asdf/one-file)))))
         (systems (remove-duplicates systems
                                     :test #'equal
                                     :key  (rcurry #'getf :provides)))
         ((&flet property-values (name)
            (loop :for system       in systems
                  :for system-name  = (second (first (getf system :provides)))
                  :for system-value = (getf system name)
                  :when system-value
                  :collect (list system-name system-value))))
         ;; Use the first value available in any of the analyzed
         ;; systems.
         ((&flet+ test-system? (system-name)
            (ppcre:scan "tests?$" system-name)))
         ((&flet system-names ()
            (loop :for system in systems
               :collect (second (first (getf system :provides))))))
         ((&flet system-names-if (predicate)
            (remove-if (complement predicate) (system-names))))
         ((&flet property-value/first (name)
            (second (first (property-values name)))))
         ((&flet maybe-property/first (name)
            (when-let ((value (property-value/first name)))
              `(,name ,value))))
         ;; Combine the values in the analyzed systems.
         ((&flet property-value/merge-persons (name)
            (rosetta-project.model.resource:merge-persons!
             (reduce #'append (property-values name) :key #'second))))
         ((&flet maybe-property/merge-persons (name)
            (when-let ((value (property-value/merge-persons name)))
              `(,name ,value))))
         ((&flet property-value/dependencies (name)
            (merge-dependencies
             (reduce #'append (property-values name) :key #'second))))
         ;; Combine descriptions of analyzed systems.
         ((&flet maybe-property/description (name)
            (let ((values (remove-if #'test-system? (property-values name)
                                     :key #'first)))
              (case (length values)
                (0 nil)
                (1 `(,name ,(second (first values))))
                (t `(,name ,(format nil "The project contains the following systems:~
                                         ~2%~
                                         ~{~{~A: ~A~}~^~2%~}"
                                    (sort values #'string< :key #'first))))))))
         ;; Compute required and provided systems.
         (requires (property-value/dependencies :requires))
         (provides (property-value/dependencies :provides)))
    `(:natures               (,kind)
      :provides              ,provides
      :requires              ,(effective-requires requires provides)
      :programming-languages ("Common Lisp")
      :systems               ,(system-names-if (complement #'test-system?))
      :test-systems          ,(system-names-if #'test-system?)
      ,@(maybe-property/description   :description)
      ,@(maybe-property/merge-persons :authors)
      ,@(maybe-property/merge-persons :maintainers)
      ,@(maybe-property/first         :license))))

;;; Utility functions

(defun %extract-system-definition-forms (pathname)
  (let+ ((result   '())
         (packages '())
         (next-macroexpand-hook *macroexpand-hook*)
         ((&flet next (function form env)
            (when next-macroexpand-hook
              (funcall next-macroexpand-hook function form env))))
         ((&flet on-macro-expansion (function form env)
            (typecase form
              ;; Record definitions of not currently existing packages
              ;; for later deletion in a cleanup step.
              ((cons (eql defpackage)) ; TODO not portable, i guess
               (let ((name (second form)))
                 (unless (find-package name)
                   (push name packages))
                 (next function form env)))
              ((cons (eql asdf:defsystem))
               (push form result)
               nil)
              ((cons (eql defmethod))
               nil)
              (t
               (next function form env)))))
         (*macroexpand-hook* #'on-macro-expansion))
    (unwind-protect
         #.(if (uiop:version<= (asdf:asdf-version) "3.3.0")
               (read-from-string
                "(handler-bind ((warning #'muffle-warning)) ; TODO silence everything?
                   (asdf::load-asd pathname))")
               (read-from-string
                "(let ((asdf::*source-registry*    nil)
                       (asdf::*registered-systems* (copy-hash-table
                                                    asdf::*registered-systems*))
                       (asdf::*asdf-session*       nil))
                   (asdf/system-registry:clear-registered-systems)
                   (asdf:initialize-source-registry
                    '(:source-registry :ignore-inherited-configuration))
                   (handler-bind ((warning #'muffle-warning)) ; TODO silence everything?
                     (asdf::load-asd pathname)))"))
      (mapc (lambda (package)
              (log:debug "~@<Deleting package ~S~@:>" package)
              (ignore-errors (delete-package package)))
            packages))
    (nreverse result)))
