;;;; util.lisp --- Utilities used in the model.project module.
;;;;
;;;; Copyright (C) 2017, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.model.project)

(defun parse-dependency-spec (spec)
  (flet ((result (nature target &optional version)
           (list* (make-keyword (string-upcase nature))
                  target
                  (etypecase version
                    (null   '())
                    (cons   version)
                    (string (list (parse-version version)))))))
    (optima:match spec
      ;; Legacy syntax: [ "NATURE", "TARGET", VERSION ]
      ((list* (and nature (type string))
              (and target (type string))
              (or (list (and version (type string))) '()))
       (result nature target version))

      ;; New syntax:
      ;; nature: NATURE
      ;; target: TARGET
      ;; version: VERSION
      ((and (assoc :nature (and nature (type string)))
            (assoc :target (and target (type string)))
            (or (assoc :version (and version (type string)))
                (and)))
       (check-keys spec '((:nature  t   string)
                          (:target  t   string)
                          (:version nil string)))
       (result nature target version))

      (otherwise
       (object-error
        (list (list spec "specified here" :error))
        "~@<Dependency specification must a list of two or three ~
         elements or a dictionary with keys \"nature\", \"target\" and ~
         optionally \"version\".~@:>")))))

(defun parse-include-spec (spec)
  (optima:match spec

    ;; Legacy syntax variants
    ((list* (and name (type string)) versions)
     (flet ((parse-version (spec)
              (optima:match spec
                ((type string)
                 (cons spec '()))
                ((list (and name (type string))
                       (and parameters (type list)))
                 (cons name parameters))
                (otherwise
                 (object-error
                  (list (list spec "specified here" :error))
                  "~@<Version is neither a string nor a list of a ~
                   string followed by a dictionary of ~
                   parameters.~@:>")))))
       (values name (map 'list #'parse-version versions))))

    ;; Current syntax for a single version
    ((optima:guard (type string) (position #\@ spec)) ; TODO check length
     (let* ((index   (position #\@ spec))
            (name    (string-right-trim '(#\Space) (subseq spec 0 index)))
            (version (string-right-trim '(#\Space) (subseq spec (1+ index)))))
       (setf (location-of name)    (location-of spec)
             (location-of version) (location-of spec))
       (values name (list (cons version nil)))))

    ((and (assoc :name    (and name    (type string)))
          (assoc :version (and version (type string)))
          (or (assoc :parameters (and parameters (type list)))
              (and)))
     (check-keys spec '((:name       t   string)
                        (:version    t   string)
                        (:parameters nil list)))
     (values name (list (cons version parameters))))

    ;; Current syntax for multiple versions
    ((and (assoc :name     (and name     (type string)))
          (assoc :versions (and versions (type list))))
     (check-keys spec '((:name t string) (:versions t list)))
     (flet ((parse-version (spec)
              (optima:match spec
                ((and (assoc :version (and version (type string)))
                      (or (assoc :parameters (and parameters (type list)))
                          (and)))
                 (check-keys spec '((:version    t   string)
                                    (:parameters nil list)))
                 (cons version parameters))
                (otherwise
                 (object-error
                  (list (list spec "specified here" :error))
                  "~@<Project version entry is not a dictionary with ~
                   keys \"version\" and optionally ~
                   \"parameters\".~:@>")))))
       (values name (map 'list #'parse-version versions))))

    (otherwise
     (object-error
      (list (list spec "specified here" :error))
      "~@<Project entry is neither a list consisting of a project name ~
       followed by one or more (parametrized) project versions nor a ~
       string of the form NAME@VERSION nor a dictionary with keys ~
       \"name\" and \"version\" or \"versions\".~:@>"))))
