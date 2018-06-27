;;;; util.lisp --- Utilities used in the api module.
;;;;
;;;; Copyright (C) 2012-2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.api)

(defmacro define-enum-type (name-and-options &body names-and-values)
  (let+ ((names '())

         ((name &key (test 'string=) (deftype? t))
          (ensure-list name-and-options))

         (value->name-clauses '())
         ((&flet value->name (name values)
            (push (if (length= 1 values)
                      `((,test value ,(first values))
                        ,name)
                      `((member value '(,@values) :test #',test)
                        ,name))
                  value->name-clauses)))

         (name->value-clauses '())
         ((&flet name->value (name values)
            (push `((,name) ,(first values))
                  name->value-clauses)))

         ((&flet+ clause ((name &rest values))
            (push name names)
            (value->name name values)
            (name->value name values))))
    (map nil #'clause names-and-values)
    `(progn
       ,@(when deftype?
           `((deftype ,name ()
               '(member ,@names))))

       (defmethod xloc:xml-> ((value string)
                              (type  (eql ',name))
                              &key &allow-other-keys)
         (cond ,@value->name-clauses
               (t
                (error "~@<Unknown ~S value: ~S.~@:>"
                       ',name value))))

       (defmethod xloc:->xml ((value symbol)
                              (dest  (eql 'string))
                              (type  (eql ',name))
                              &key &allow-other-keys)
         (ecase value
           ,@name->value-clauses)))))
