;;;; mixins.lisp --- Mixins provided by the deployment module.
;;;;
;;;; Copyright (C) 2018, 2019, 2022 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:build-generator.deployment)

(defclass command-mixin ()
  ((%command :initarg  :command
             :reader   command))
  (:default-initargs
   :command (more-conditions:missing-required-initarg 'command-mixin :command)))

(defmethod print-items:print-items append ((object command-mixin))
  (multiple-value-bind (command shortened?)
      (util:maybe-truncate (command object))
    `((:command "~{\"~A~:[~;…~]\"~}" ,(list command shortened?)))))
