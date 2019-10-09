;;;; target.lisp --- Deployment target for Jenkins.
;;;;
;;;; Copyright (C) 2018, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:build-generator.deployment.jenkins)

(defclass target ()
  ((%delete-other?        :initarg :delete-other?
                          :reader  delete-other?)
   (%delete-other-pattern :initarg :delete-other-pattern
                          :reader  delete-other-pattern)))

(service-provider:register-provider/class
 'deploy:target :jenkins :class 'target)
