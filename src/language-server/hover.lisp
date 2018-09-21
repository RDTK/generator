;;;; hover.lisp --- TODO.
;;;;
;;;; Copyright (C) 2016, 2017, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.language-server)

(defclass variable-hover-contributor () ())

(defmethod contrib:hover-contribution ((workspace   t)
                                       (document    t)
                                       (context     variable-value-context)
                                       (contributor variable-hover-contributor))
  (let ((variable-location (variable-location context))
        (variable-node     (variable-node context)))
    (values
     (list (format nil "Type: ~A" (jenkins.model.variables:variable-info-type variable-node))
           (or (jenkins.model.variables:variable-info-documentation variable-node)
               "«undocumented variable»"))
     (text.source-location:range variable-location) )))
