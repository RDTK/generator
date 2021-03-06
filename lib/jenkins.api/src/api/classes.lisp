;;;; classes.lisp --- Classes used by the api module.
;;;;
;;;; Copyright (C) 2012-2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.api)

;;; `node' class

(define-model-class node ()
  ((name        :type      string)
   (description :type      string)
   (host        :type      string
                :xpath     "launcher/host/text()")
   (mode        :type      keyword)
   (label       :type      (list/space string)
                :xpath     "label/text()")
   (environment :type      tree-map/plist
                :xpath     "/slave/nodeProperties/hudson.slaves.EnvironmentVariablesNodeProperty/envVars/tree-map"))
  (:root?     t)
  (:name-slot nil))

(defmethod print-object ((object node) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A ~:[off~;on~]line"
            (name object) (online? object))))

;;; `item' class (Queue items)

(define-model-class item ()
    ((job-name :type  string
               :xpath "task/name/text()"))
  (:root? t))

(defmethod job ((item item) &key &allow-other-keys)
  (job (job-name item)))
