;;;; mps.lisp --- Analysis of MPS projects.
;;;;
;;;; Copyright (C) 2017, 2018, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:build-generator.analysis)

(define-constant +mps-plugin-build-files+
    :mps-plugin-build-files)

(define-constant +mps-plugin-build-file-nature+
    :mps-plugin-build-file)

(define-constant +mps-artifacts-prefix+
    "artifacts."
  :test #'string=)

(define-constant +mps-project-name+
    "/project/@name"
  :test #'string=)

(define-constant +mps-property+
    "/project/property[@name and @location]"
  :test #'string=)

(define-constant +mps-dependency-sources+
    "/project/target[@name='fetchDependencies']/unzip/@src"
  :test #'string=)

(defmethod xloc:xml-> ((value stp:element)
                       (type  (eql 'list/property))
                       &key inner-types)
  (declare (ignore inner-types))
  (xloc:with-locations-r/o ((name     "@name")
                            (location "@location"))
      value
    (list name location)))

(defmethod analyze :around ((source pathname)
                            (kind   (eql +mps-plugin-build-file-nature+))
                            &key)
  (if (uiop:directory-pathname-p source)
      (analyze source +mps-plugin-build-files+)
      (call-next-method)))

(defmethod analyze ((source pathname)
                    (kind   (eql +mps-plugin-build-files+))
                    &key
                    (build-file-pattern "*build*.xml"))
  (let+ ((source  (truename source))
         (files   (directory (merge-pathnames build-file-pattern source)))
         (results (mapcan
                   (lambda (file)
                     (with-simple-restart
                         (continue "~@<Skip file \"~A\".~@:>" file)
                       (list (list (enough-namestring file source)
                                   (analyze file +mps-plugin-build-file-nature+)))))
                   files))
         ((&flet property-values (name)
            (map 'list (compose (rcurry #'getf name) #'second) results)))
         ;;
         ((&flet property-value/union (name &key (test #'equal))
            (reduce (rcurry #'union :test test) (property-values name)
                    :initial-value '())))
         ;; Combine the values in the analyzed packages.
         ((&flet property-value/dependencies (name)
            (merge-dependencies
             (reduce #'append (property-values name))))))
    `(:natures               ,(property-value/union :natures :test #'eq)
      :provides              ,(property-value/dependencies :provides)
      :requires              ,(property-value/dependencies :requires)
      :programming-languages ,(property-value/union :programming-languages
                                                    :test #'eq)
      :home-variables        ,(property-value/union :home-variables)
      :plugin-build-files    ,(map 'list #'first results))))

(defmethod analyze ((source pathname)
                    (kind   (eql +mps-plugin-build-file-nature+))
                    &key)
  (let ((document (cxml:parse source (stp:make-builder))))
    (xloc:with-locations-r/o
        ((name                                   +mps-project-name+)
         ((:val properties :type 'list/property) +mps-property+
                                                 :if-multiple-matches :all)
         (dependencies                           +mps-dependency-sources+
                                                 :if-multiple-matches :all))
        document
      (let+ (((&flet find-property (name)
                (or (find name properties :test #'string= :key #'first)
                    (error "~@<Could not find property \"~A\".~@:>" name))))
             ((&flet reference-variable (expression)
                (let* ((open  (or (position #\{ expression)
                                  (error "Could not find ~C in \"~A\"."
                                         #\{ expression)))
                       (close (or (position #\} expression :start open)
                                  (error "Could not find ~C in \"~A\"."
                                         #\} expression))))
                  (subseq expression (1+ open) close))))
             ((&flet resolve-dependency (source)
                (let* ((name     (reference-variable source))
                       (property (find-property name)))
                  (unless (starts-with-subseq +mps-artifacts-prefix+ name)
                    (error "~@<\"~A\" does not start with \"~A\".~@:>"
                           name +mps-artifacts-prefix+))
                  (list (list +mps-plugin-build-file-nature+
                              (subseq name (length +mps-artifacts-prefix+)))
                        (reference-variable (second property))))))
             ((&flet resolve-dependencies (sources)
                (mapcan (lambda (source)
                          (with-simple-restart
                              (continue "~@<Skip dependency \"~A\".~@:>" source)
                            (list (resolve-dependency source))))
                        sources)))
             (dependencies (resolve-dependencies dependencies)))
        `(:natures               (,+mps-plugin-build-file-nature+)
          :provides              ((,+mps-plugin-build-file-nature+ ,name))
          :requires              ,(map 'list #'first dependencies)
          :programming-languages (:mps)
          :home-variables        ,(map 'list #'second dependencies))))))
