;;;; protocol.lisp ---
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.analysis)

(defgeneric analyze (source kind &key &allow-other-keys)
  (:documentation
   "TODO"))

(defmethod analyze :around ((source t) (kind t) &key &allow-other-keys)
  (with-condition-translation (((error analysis-error)
                                :specification source))
    (call-next-method)))
