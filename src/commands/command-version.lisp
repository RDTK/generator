;;;; command-version.lisp --- Command for printing relevant versions.
;;;;
;;;; Copyright (C) 2017, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.project.commands)

(defclass version ()
  ((changelog-count :initarg  :changelog-count
                    :type     (or boolean positive-integer)
                    :reader   changelog-count
                    :initform nil
                    :documentation
                    #.(format nil "Number of releases for which a list ~
                       of changes should be printed.")))
  (:documentation
   "Print the version of this program and some components."))

(service-provider:register-provider/class
 'command :version :class 'version)

(jenkins.project.commandline-options:define-option-mapping
    (*command-schema* "version")
  (("-c" "--changelog") "changelog-count" "COUNT"))

(defmethod command-execute ((command version))
  (let* ((stream    *standard-output*)
         (version   (generator-version))
         (versions  `(("build-generator"           ,version)
                      ("asdf"                      ,(asdf:asdf-version))
                      (,(lisp-implementation-type) ,(lisp-implementation-version))))
         (max-width (reduce #'max versions
                            :initial-value 0
                            :key           (compose #'length #'first))))
    (format stream "~{~{~V:A ~:[~{~D.~D~^.~D~^-~A~}~;~A~]~}~&~}"
            (loop :for (name version) :in versions
                  :collect `(,max-width ,name ,(stringp version) ,version)))

    (when-let ((count (changelog-count command)))
      (let ((count (if (eq count t) nil count)))
        (format stream "~2%")
        (print-changelog (changelog :count count) :stream stream)))))
