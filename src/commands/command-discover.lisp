;;;; command-discover.lisp --- Discover a Jenkins instances on the network.
;;;;
;;;; Copyright (C) 2017, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.project.commands)

(defclass discover ()
  ()
  (:documentation
   "Discover Jenkins instance on the local network and report."))

(service-provider:register-provider/class
 'command :discover :class 'discover)

(jenkins.project.commandline-options:define-option-mapping
    (*command-schema* "discover"))

(defconstant +jenkins-discover-port+ 33848)

(defmethod command-execute ((command discover))
  (let+ ((port +jenkins-discover-port+)
         ((&flet discover (socket address)
            (setf (sb-bsd-sockets:sockopt-broadcast socket) t)
            (sb-bsd-sockets:socket-send socket (nibbles:octet-vector) 0
                                        :address address)
            (warn "socket-receive does not respect deadlines")
            (loop :with buffer = (make-string 64000)
                  :for  (nil length address)
                        = (handler-case
                              (sb-sys:with-deadline (:seconds 5)
                                (multiple-value-list
                                 (sb-bsd-sockets:socket-receive socket buffer nil)))
                            (sb-sys:deadline-timeout ()
                              (return results)))
                  :collect (list address (subseq buffer 0 length)) :into results)))
         ((&flet+ report ((address description))
            (format t "Jenkins at ~A:~%~A~%"
                    address description))))
    (lparallel:plet ((v4 (discover (make-instance 'sb-bsd-sockets:inet-socket :type :datagram :protocol :ip)
                                   (list (sb-bsd-sockets:make-inet-address "129.70.135.255") port)))
                     (v6 (discover (make-instance 'sb-bsd-sockets:inet6-socket :type :datagram :protocol :ip)
                                   (list (sb-bsd-sockets:make-inet6-address "ff02::1") port))))
      (map nil #'report (append v4 v6)))))
