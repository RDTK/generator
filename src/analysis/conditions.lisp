;;;; conditions.lisp --- Conditions used in the analysis module.
;;;;
;;;; Copyright (C) 2014-2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:build-generator.analysis)

(define-condition analysis-condition (chainable-condition)
  ((specification :initarg  :specification
                  :reader   analysis-condition-specification
                  :documentation
                  "Stores the specification, the analysis resulted in
                   the condition being signaled."))
  (:default-initargs
   :specification (missing-required-initarg 'analysis-condition :specification))
  (:documentation
   "Subclasses of this condition are signaled to indicate certain
    condition during the analysis of specifications."))

(define-condition analysis-error (error
                                  analysis-condition)
  ()
  (:report
   (lambda (condition stream)
     (format stream "~@<Error during analysis of ~A~
                     ~/more-conditions:maybe-print-cause/~@:>"
             (analysis-condition-specification condition)
             condition)))
  (:documentation
   "This error is signaled when an error is encountered during the
    analysis of a specification."))

(define-condition repository-access-error (analysis-error)
  ()
  (:report
   (lambda (condition stream)
     (format stream "~@<Error accessing repository \"~A\".~
                     ~/more-conditions:maybe-print-cause/~@:>"
             (analysis-condition-specification condition)
             condition)))
  (:documentation
   "Signaled when a repository cannot be accessed."))

(define-condition repository-analysis-error (analysis-error)
  ((context-directory :initarg :context-directory
                      :reader  context-directory))
  (:default-initargs
   :context-directory (missing-required-initarg
                       'repository-analysis-error :context-directory))
  (:report
   (lambda (condition stream)
     (let* ((context-directory (context-directory condition))
            (pathname          (analysis-condition-specification condition))
            (enough-namestring (util:safe-enough-namestring
                                pathname context-directory)))
       (format stream "~@<Error during analysis of repository path ~
                       \"~A\".~/more-conditions:maybe-print-cause/~@:>"
                       enough-namestring condition))))
  (:documentation
   "Signaled when an error is encountered while analyzing a
    repository."))

(define-condition project-analysis-error (analysis-error)
  ()
  (:report
   (lambda (condition stream)
     (format stream "~@<Error during analysis of project ~
                     ~/print-items:format-print-items/.~
                     ~/more-conditions:maybe-print-cause/~@:>"
             (print-items:print-items
              (analysis-condition-specification condition))
             condition)))
  (:documentation
   "Signaled when an error is encountered while analyzing a
    project."))

;;; Dependency conditions

(define-condition dependency-condition ()
  ((dependency :initarg  :dependency
               :reader   dependency-condition-dependency))
  (:default-initargs
   :dependency (missing-required-initarg 'dependency-condition :dependency))
  (:documentation
   "Superclass for dependency-related conditions."))

(define-condition unfulfilled-dependency-error (error
                                                dependency-condition)
  ()
  (:documentation
   "Generic error condition for signaling unfulfilled dependency
    errors."))

(define-condition unfulfilled-project-dependency-error (unfulfilled-dependency-error)
  ((candidates :initarg  :candidates
               :reader   unfulfilled-project-dependency-candidates
               :initform '()))
  (:report
   (lambda (condition stream)
     (format stream "~@<No provider for ~S.~@[ Candidates: ~{~A~^, ~
                     ~}~]~@:>"
             (dependency-condition-dependency           condition)
             (unfulfilled-project-dependency-candidates condition))))
  (:documentation
   "This error is signaled if an unfulfilled dependency between
    projects is detected."))

(define-condition unfulfilled-platform-dependency-error (unfulfilled-dependency-error)
  ()
  (:documentation
   "This error is signaled if an unfulfilled dependency of a project
    on a platform package is detected."))
