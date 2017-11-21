;;;; workspace-methods.lisp --- TODO.
;;;;
;;;; Copyright (C) 2016, 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.language-server)

(defmethod process-method ((object workspace)
                           (method (eql :did-change-configuration))
                           &key
                             ))

(defmethod process-method ((object workspace)
                           (method (eql :did-change-watched-files))
                           &key
                             ))
