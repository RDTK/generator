;;;; ros-packages.lisp --- Analysis of multi-ROS package repositories.
;;;;
;;;; Copyright (C) 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.analysis)

(defun meta-package? (package-filename)
  (let ((document (cxml:parse package-filename (stp:make-builder))))
    (unless (xpath:node-set-empty-p
             (xpath:evaluate "package/export/metapackage" document))
      (log:info "~@<Skipping apparent meta-package ~S~@:>" package-filename)
      t)))

(defmethod analyze ((directory pathname)
                    (kind      (eql :ros-packages))
                    &key)
  (let+ ((candidates  (directory (merge-pathnames "**/package.xml" directory)))
         (packages    (remove-if #'meta-package? candidates))
         (directories (mapcar #'uiop:pathname-directory-pathname packages))
         (results     (mapcan
                       (lambda (directory)
                         (with-simple-restart (continue "Skip sub-directory ~S"
                                                        directory)
                           (list (analyze directory :ros-package))))
                       directories))
         ((&flet property-values (name)
            (loop :for result in results
               :for package-name  = (second (first (getf result :provides)))
               :for package-value = (getf result name)
               :when package-value
               :collect (list package-name package-value))))
         ;; Use the first value available in any of the analyzed
         ;; packages.
         ((&flet property-value/first (name)
            (second (first (property-values name)))))
         ((&flet maybe-property/first (name)
            (when-let ((value (property-value/first name)))
              `(,name ,value))))
         ;; Combine the values in the analyzed packages.
         ((&flet property-value/merge-persons (name)
            (rosetta-project.model.resource:merge-persons!
             (reduce #'append (property-values name) :key #'second))))
         ((&flet maybe-property/merge-persons (name)
            (when-let ((value (property-value/merge-persons name)))
              `(,name ,value))))
         ((&flet property-value/dependencies (name)
            (merge-dependencies
             (reduce #'append (property-values name) :key #'second))))
         ;; Combine descriptions of analyzed packages.
         ((&flet maybe-property/description (name)
            (let ((values (property-values name)))
              (case (length values)
                (0 nil)
                (1 `(,name ,(second (first values))))
                (t `(,name ,(format nil "The project contains the following packages:~
                                         ~2%~
                                         ~{~{~A: ~A~}~^~2%~}"
                                    (sort (remove-duplicates values :test #'equal)
                                          #'string< :key #'first))))))))
         (requires (property-value/dependencies :requires))
         (provides (property-value/dependencies :provides)))
    ;; Reduce to effectively required packages.
    (setf requires
          (set-difference
           requires provides
           :test (lambda+ ((&ign required-name &optional required-version)
                           (&ign provided-name &optional provided-version))
                   (and (string= required-name provided-name)
                        (version-matches required-version provided-version)))))
    ;; Final result.
    `(:natures  (,kind)
      :provides ,provides
      :requires ,requires
      ,@(maybe-property/description   :description)
      ,@(maybe-property/merge-persons :authors)
      ,@(maybe-property/merge-persons :maintainers)
      ,@(maybe-property/first         :license))))
