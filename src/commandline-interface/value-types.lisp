;;;; value-types.lisp --- Additional value types used by the commandline interface.
;;;;
;;;; Copyright (C) 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.project.commandline-interface)

;;; `error-policy'

(macrolet
    ((define-cause-predicate (condition-name)
       (let* ((type-name      (symbolicate '#:caused-by- condition-name))
              (predicate-name (symbolicate type-name '#:?)))
         `(progn
            (defun ,predicate-name (thing)
              (and (typep thing 'condition)
                   (util:some-cause (of-type ',condition-name) thing)))
            (deftype ,type-name ()
              '(satisfies ,predicate-name))))))

  (define-cause-predicate analysis:repository-access-error)
  (define-cause-predicate analysis:repository-analysis-error)
  (define-cause-predicate analysis:project-analysis-error)
  (define-cause-predicate analysis:unfulfilled-project-dependency-error))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *condition-types*
    '((project::object-error                          . nil)
      (project::simple-object-error                   . nil)
      (project::yaml-syntax-error                     . "syntax-error")

      (caused-by-repository-access-error              . "repository-access-error")
      (caused-by-repository-analysis-error            . "repository-analysis-error")
      (caused-by-project-analysis-error               . "project-analysis-error")
      (analysis:analysis-error                        . nil)

      (caused-by-unfulfilled-project-dependency-error . "dependency-error")

      (model:instantiation-error                      . nil)

      (model:deployment-error                         . nil)

      (jenkins.report::report-error                   . nil)))

  (defvar *error-handling-actions*
    '(:abort :fail :continue :debug)))

(deftype error-handling-action ()
  `(member ,@*error-handling-actions*))

(deftype error-policy ()
  '(cons (cons symbol error-handling-action) list))
(setf (get 'error-policy 'options::dont-expand) t)

(macrolet ((define-condition-type-rules ()
             (let+ ((rule-names '())
                    ((&flet+ condition-type-rule ((name . alias))
                       (let ((strings   (list* (string-downcase name)
                                               (when alias
                                                 (list alias))))
                             (rule-name (symbolicate '#:condition-type- name)))
                         (push rule-name rule-names)
                         `(esrap:defrule ,rule-name
                              ,(if (length= 1 strings)
                                   (first strings)
                                   `(or ,@strings))
                            (:constant ',name))))))
               `(progn
                  ,@(map 'list #'condition-type-rule *condition-types*)

                  (esrap:defrule condition-type
                      (or ,@rule-names)))))

           (define-action-rules ()
             (let+ ((rule-names '())
                    ((&flet+ action-rule (name)
                       (let ((rule-name (symbolicate '#:action- name)))
                         (push rule-name rule-names)
                         `(esrap:defrule ,rule-name
                              ,(string-downcase name)
                            (:constant ,name))))))
               `(progn
                  ,@(map 'list #'action-rule *error-handling-actions*)

                  (esrap:defrule action
                      (or ,@rule-names))))))

  (define-condition-type-rules)

  (define-action-rules))

(esrap:defrule one
    (and condition-type "=>" action ":")
  (:destructure (condition-type arrow action colon)
    (declare (ignore arrow colon))
    (cons condition-type action)))

(esrap:defrule default
    action
  (:lambda (action)
    (cons t action)))

(esrap:defrule multi
    (and (* one) default)
  (:destructure (actions default)
    (append actions (list default))))

(defmethod options:raw->value-using-type ((schema-item t)
                                          (raw         string)
                                          (type        (eql 'error-policy))
                                          &key inner-type)
  (declare (ignore inner-type))
  (esrap:parse 'multi raw))

(defmethod options:value->string-using-type ((schema-item t)
                                             (value       cons)
                                             (type        (eql 'error-policy))
                                             &key inner-type)
  (declare (ignore inner-type))
  (with-output-to-string (stream)
    (map nil (lambda+ ((condition-type . action))
               (let ((default? (eq condition-type t))
                     (alias    (assoc-value *condition-types* condition-type)))
                 (format stream "~:[~(~A~)=>~;~*~]~(~A~)~3:*~:[:~;~]"
                         default? (or alias condition-type) action)))
         value)))
