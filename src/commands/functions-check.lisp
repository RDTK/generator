;;;; functions-check.lisp --- Functions for checking recipes.
;;;;
;;;; Copyright (C) 2017, 2018, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.project.commands)

(defun check-distribution-access (distributions)
  (mapcan (lambda (distribution)
            (with-simple-restart
                (continue "~@<Skip distribution ~A.~@:>" distribution)
              (let+ (((&values access? problem)
                      (model:check-access distribution t)))
                (cond (access?
                       (list distribution))
                      (problem
                       (error problem))
                      (t
                       (error "~@<Unsuitable access declaration in ~
                               distribution ~A.~@:>"
                              distribution))))))
          distributions))

(defun unresolved-platform-requirements
    (distributions
     &key
     (platform (multiple-value-list (analysis:current-platform))))
  (let ((installed-packages (analysis:installed-packages))
        (requirements       (project:platform-requires distributions platform)))
    (log:info "~@<Found ~:D installed package~:P~@:>"
              (length installed-packages))
    (log:debug "~@<Found ~:D platform requirement~:P: ~{~A~^ ~}~@:>"
               (length requirements) requirements)
    (when (and platform installed-packages)
      (values (with-sequence-progress (:check-platform-requirements requirements)
                (remove-if (lambda (requirement)
                             (progress "~A" requirement)
                             (find requirement installed-packages
                                   :test #'string= :key #'first))
                           requirements))
              platform))))

(defun report-platform-requirements (requirements platform &key label)
  (let ((requirements (sort (copy-list requirements) #'string<)))
    (format t "~@<Found ~:D ~@[~A ~]platform requirement~:*~:P~* ~
               for ~{~A~^ ~}:~>~@
               ~@
               ~@[~2@T~{~<~T\\~%~2@T~1,:;~A~>~^ ~}~]~%"
            (length requirements) label platform requirements)))
