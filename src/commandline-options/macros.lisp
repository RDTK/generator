;;;; macros.lisp --- Macros provided by the commandline-options module.
;;;;
;;;; Copyright (C) 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.project.commandline-options)

(defmacro define-option-mapping ((schema context) &body clauses)
  (let+ (((&flet+ register-info ((designators option-name
                                  &optional argument-name mandatory?))
            (let* ((designators (ensure-list designators))
                   (positional? (every (of-type 'positional-option-designator)
                                       designators))
                   (named?      (every (of-type 'named-option-designator)
                                       designators))
                   (class       (cond
                                  (positional?
                                   'positional-option-info)
                                  ((and named? (not argument-name))
                                   'named-without-argument-option-info)
                                  (named?
                                   'named-with-argument-option-info)
                                  (t
                                   (error "~@<Invalid combination of ~
                                           designators: ~:S~@:>"
                                          designators)))))
              `(let* ((option (configuration.options:find-option
                               (list ,context ,option-name) ,schema)))
                 (register-option
                  ,context (make-instance ',class
                                          :option        option
                                          :designators   '(,@designators)
                                          :argument-name ,argument-name
                                          :mandatory?    ,mandatory?)))))))
    `(progn
       (setf (find-options ,context)
             (find-options ,context :if-does-not-exist '()))
       ,@(map 'list #'register-info clauses))))
