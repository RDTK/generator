;;;; asdf.lisp --- Analysis of ASDF projects.
;;;;
;;;; Copyright (C) 2013, 2014, 2016 Jan Moringen
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
         ((&labels dependency-name (dependency)
            (etypecase dependency
              (cons (dependency-name (second dependency)))
              (t    (string-downcase dependency)))))
         ((&labels process-version (spec)
            (typecase spec
              ((cons (member :read-file-form :read-file-file))
               (process-version (read-file-* spec)))
              (string
               (parse-version spec)))))
         ((&flet dependency->list (dependency)
            (etypecase dependency
              ((cons (eql :version))
               (list :asdf
                     (string-downcase (second dependency))
                     (process-version (third dependency))))
              ((or string symbol)
               (list :asdf (string-downcase dependency))))))
         (system-systems (mappend #'ql-dist:provided-systems (ql:system-list)))
         ((&flet systems-system? (spec)
            (find (dependency-name spec) system-systems
                  :test #'equal :key #'ql-dist:name)))
         ((&flet effective-dependencies (depends-on)
            (remove-if #'systems-system? depends-on)))
         ((&flet+ process-system-form ((&ign name
                                        &key
                                        version
                                        license
                                        author
                                        maintainer
                                        description
                                        depends-on
                                        &allow-other-keys))
            (append
             (list :versions `((:main . ,version))
                   :provides `((:asdf
                                ,(string-downcase name)
                                ,(process-version version)))
                   :requires (mapcar #'dependency->list
                                     (effective-dependencies depends-on)))
             (when description `(:description ,description))
             (when author      `(:authors (,author)))
             (when maintainer  `(:maintainers (,maintainer)))
             (when license     `(:license ,license))))))
    (mapcar #'process-system-form (%extract-system-definition-forms file))))

(defmethod analyze ((directory pathname)
                    (kind      (eql :asdf))
                    &key)
  (let+ ((systems
          (iter (for file in (find-files (merge-pathnames "*.asd" directory)))
                (log:info "~@<Analyzing ~S.~@:>" file)
                (restart-case
                    (appending (analyze file :asdf/one-file))
                  (continue (&optional condition)
                    :report (lambda (stream)
                              (format stream "~@<Skip file ~S.~@:>"
                                      file))
                    (declare (ignore condition))))))
         ((&flet property-values (name)
            (loop :for system       in systems
                  :for system-name  = (second (first (getf system :provides)))
                  :for system-value = (getf system name)
                  :when system-value
                  :collect (list system-name system-value))))
         ;; Use the first value available in any of the analyzed
         ;; systems.
         ((&flet property-value/first (name)
            (second (first (property-values name)))))
         ((&flet maybe-property/first (name)
            (when-let ((value (property-value/first name)))
              `(,name ,value))))
         ;; Append the value in the analyzed systems.
         ((&flet property-value/append (name)
            (remove-duplicates (reduce #'append (property-values name)
                                       :key #'second)
                               :test #'equal)))
         ((&flet maybe-property/append (name)
            (when-let ((value (property-value/append name)))
              `(,name ,value))))
         ;; Combine descriptions of analyzed systems.
         ((&flet maybe-property/description (name)
            (let ((values (remove-if (lambda+ ((&whole foo system-name &ign))
                                       (ppcre:scan "tests?$" system-name))
                                     (property-values name))))
              (case (length values)
                (0 nil)
                (1 `(,name ,(second (first values))))
                (t `(,name ,(format nil "The project contains the following systems:~
                                         ~2%~
                                         ~{~{~A: ~A~}~^~2%~}"
                                    (sort values #'string< :key #'first))))))))
         ;; Compute required and provided systems.
         (requires (property-value/append :requires))
         (provides (property-value/append :provides)))
    ;; Reduce to effectively required system.
    (setf requires (set-difference
                    requires provides
                    :test (lambda+ ((&ign required-name &optional required-version)
                                    (&ign provided-name &optional provided-version))
                            (and (string= required-name provided-name)
                                 (version-matches required-version provided-version)))))
    (append (list :versions (property-value/first :versions)
                  :provides provides
                  :requires requires)
            (maybe-property/description :description)
            (maybe-property/append      :authors)
            (maybe-property/append      :maintainers)
            (maybe-property/first       :license))))

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
         (progn
           (handler-bind ((warning #'muffle-warning)) ; TODO silence everything?
             (asdf::load-asd pathname))
           (nreverse result))
      (mapc (lambda (package)
              (log:debug "~@<Deleting package ~S~@:>" package)
              (ignore-errors (delete-package package)))
            packages))))
