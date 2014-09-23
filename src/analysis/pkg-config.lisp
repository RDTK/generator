;;;; pkg-config.lisp --- Analyze pkg-config files.
;;;;
;;;; Copyright (C) 2012, 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.analysis)

(defmethod analyze ((source pathname) (kind (eql :pkg-config))
                    &key)
  (with-open-stream (stream (apply #'open source (safe-external-format-argument)))
    (analyze stream kind)))

(defmethod analyze ((source stream) (kind (eql :pkg-config))
                    &key)
  (let+ ((name)
         (version)
         ((&flet parse-requires (value)
            (iter (for spec in (split-sequence-if
                                (rcurry #'member '(#\Space #\Tab)) value
                                :remove-empty-subseqs t))
                  (collect `(:pkg-config ,spec))))))
   (iter (for line in-stream source :using #'read-line)
         (ppcre:register-groups-bind (key value)
             ("[ \\t]*([^:]+):[ \\t]*([^ \\t#]*)" line)
           (cond
             ((string= key "Name")
              (setf name value))
             ((string= key "Version")
              (setf version (parse-version value)))
             ((string= key "Requires")
              (appending (parse-requires value) :into requires))))
         (finally (return `(:provides ((:pkg-config ,name ,version))
                            :requires ,requires))))))
