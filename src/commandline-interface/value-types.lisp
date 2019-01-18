(cl:in-package #:jenkins.project.commandline-interface)

;;; `error-policy'

(defun caused-by-unfulfilled-project-dependency-error? (thing)
  (and (typep thing 'condition)
       (typep (root-cause thing)
              'jenkins.analysis:unfulfilled-project-dependency-error)))

(deftype caused-by-unfulfilled-project-dependency-error ()
  `(satisfies caused-by-unfulfilled-project-dependency-error?))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *condition-types*
    '((jenkins.model.project::object-error            . nil)
      (jenkins.model.project::simple-object-error     . nil)
      (jenkins.model.project::yaml-syntax-error       . "syntax-error")
      (jenkins.analysis:analysis-error                . nil)
      (caused-by-unfulfilled-project-dependency-error . "dependency-error")
      (jenkins.model:instantiation-error              . nil)
      (jenkins.report::report-error                   . nil)))

  (defvar *error-handling-actions*
    '(:abort :fail :continue :debug)))

(deftype error-handling-action ()
  `(member ,@*error-handling-actions*))

(deftype error-policy ()
  '(cons (cons symbol error-handling-action) list))
(setf (get 'error-policy 'configuration.options::dont-expand) t)

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

           (define-action-rules (&rest names)
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

(esrap:parse 'multi "dependency-error=>abort:object-error=>abort:instantiation-error=>abort:abort")

(defmethod configuration.options:raw->value-using-type
    ((schema-item t)
     (raw         string)
     (type        (eql 'error-policy))
     &key inner-type)
  (declare (ignore inner-type))
  (esrap:parse 'multi raw))

(defmethod configuration.options:value->string-using-type
    ((schema-item t)
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

(configuration.options:raw->value-using-type
 nil
 "object-error=>abort:instantiation-error=>abort:abort"
 'error-policy)

(configuration.options:raw->value-using-type
 nil
 "abort"
 'error-policy)

(configuration.options:value->string-using-type
 nil
 '((error . :fail) (t . :abort))
 'error-policy)

(configuration.options:value->string-using-type
 nil
 '((caused-by-unfulfilled-project-dependency-error . :fail) (t . :abort))
 'error-policy)
