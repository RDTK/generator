;;;; protocol.lisp --- Protocol for the analysis module.
;;;;
;;;; Copyright (C) 2012-2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.analysis)

;;; Analysis protocol

(defgeneric analyze (source kind &key &allow-other-keys)
  (:documentation
   "Analyze the project in SOURCE assuming project kind KIND.

    * If SOURCE is a pathname, return analysis results in form of a
      plist containing at least the keys `:provides' and `:requires'.

      * If SOURCE is a pathname and KIND is `:guess', an attempt is
        made to guess the project kind from SOURCE.

    * If SOURCE is a `puri:uri', return a list containing such
      analysis results, the elements corresponding to revisions in the
      repository designated by the URI SOURCE. Elements are of the
      form

        ((:version \"VERSION\" :branch \"BRANCH\" :commit \"COMMIT\")
         . RESULTS-PLIST)

      .

      * If SOURCE is a `puri:uri' and KIND is `:guess', an attempt is
        made to guess the version control system from SOURCE and the
        project kind from the content of SOURCE."))

(defvar *outermost?* t)

(defmethod analyze :around ((source t) (kind t) &key project &allow-other-keys)
  (labels ((do-it ()
             (let ((result (multiple-value-list (call-next-method))))
               (log:debug "~@<Analysis result for ~A ~A: ~S~@:>"
                          source kind result)
               (apply #'values result)))
           (do-it/generic-translation ()
             (with-condition-translation (((error analysis-error)
                                           :specification source))
               (let ((*outermost?* nil))
                 (do-it)))))
    (cond ((not *outermost?*)
           (do-it))
          ((not project)
           (do-it/generic-translation))
          (t
           (with-condition-translation (((error project-analysis-error)
                                         :specification project))
             (do-it/generic-translation))))))
