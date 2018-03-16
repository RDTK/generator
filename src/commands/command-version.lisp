;;;; command-version.lisp --- Command for printing relevant versions.
;;;;
;;;; Copyright (C) 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.project.commands)

(defclass version ()
  ()
  (:documentation
   "Print the version of this program and some components."))

(service-provider:register-provider/class
 'command :version :class 'version)

(jenkins.project.commandline-options:define-option-mapping
    (*command-schema* "version"))

(defmethod command-execute ((command version))
  (let* ((version   (generator-version))
         (versions  `(("build-generator"           ,version)
                      ("asdf"                      ,(asdf:asdf-version))
                      (,(lisp-implementation-type) ,(lisp-implementation-version))))
         (max-width (reduce #'max versions
                            :initial-value 0
                            :key           (compose #'length #'first))))
    (format *standard-output* "~{~{~V:A ~:[~{~D.~D~^.~D~^-~A~}~;~A~]~}~&~}"
            (loop :for (name version) :in versions
               :collect `(,max-width ,name ,(stringp version) ,version)))))
