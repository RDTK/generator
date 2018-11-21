;;;; build.lisp --- Build model class.
;;;;
;;;; Copyright (C) 2012-2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.api)

;;; `build' class
;;;
;;; Aggregated classes:
;;;
;;; * `action' interface and its implementations

(define-interface-implementations
    (action :class-location (xloc:val "@*[local-name() = '_class']"))

  ((git-build-data "hudson.plugins.git.util.BuildData")
   ((last-built-revision/sha1 :type  string
                              :xpath "lastBuiltRevision/SHA1/text()"))
   (:name-slot last-built-revision/sha1))

  ((subversion-change-log-set "hudson.scm.SubversionChangeLogSet")
   ((revision :type  string
              :xpath "revision/revision/text()"))
   (:name-slot revision)))

;; TODO define-interface-implementations should do this
(macrolet ((define-of-type-methods (interface
                                    &optional
                                    (plural (symbolicate interface '#:s)))
             (let ((name/list (symbolicate plural    '#:-of-type))
                   (name/one  (symbolicate interface '#:-of-type)))
               `(progn
                  (defmethod ,name/list (type job)
                    (remove-if-not (of-type type) (,plural job)))

                  (defmethod ,name/one (type job)
                    (find-if (of-type type) (,plural job)))))))

  (define-of-type-methods action))

(define-model-class build ()
  ((building?  :type  boolean
               :xpath "building/text()")
   (slave-name :type  string
               :xpath "builtOn/text()")
   (result     :type  keyword
               :xpath "result/text()")
   (actions    :type  action
               ;; Treat Subversion changeSet as action even though it
               ;; isn't.
               :xpath ("*[(local-name() = 'action' or local-name() = 'changeSet')
                        and @*[local-name() = '_class']]"
                       :if-multiple-matches :all)))
  (:get-func (lambda (id) (build-config id))))

(defmethod job ((build build) &key &allow-other-keys)
  (job (first (split-sequence #\/ (id build)))))

(defmethod slave ((build build) &key &allow-other-keys)
  (node (slave-name build)))

(defmethod failed? ((build build))
  (eq (result build) :failure))

(defmethod print-object ((object build) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A ~A"
            (id object) (if (building? object)
                            :in-progress
                            (result object)))))
