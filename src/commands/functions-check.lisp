;;;; functions-check.lisp --- Functions for checking recipes.
;;;;
;;;; Copyright (C) 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.project.commands)

(defun check-distribution-access (distributions)
  (mapcan (lambda (distribution)
            (with-simple-restart
                (continue "~@<Skip distribution ~A.~@:>" distribution)
              (let+ (((&values access? problem)
                      (check-access distribution t)))
                (cond
                  (access?
                   (list distribution))
                  (problem
                   (error problem))
                  (t
                   (error "~@<Unsuitable access declaration in ~
                           distribution ~A.~@:>"
                          distribution))))))
          distributions))

(defun check-platform-requirements
    (distributions
     &key
       (platform (multiple-value-list (jenkins.analysis:current-platform))))
  (let ((installed-packages (jenkins.analysis:installed-packages))
        (requirements       (platform-requires distributions platform)))
    (log:info "~@<Found ~:D installed package~:P~@:>"
              (length installed-packages))
    (log:debug "~@<Found ~:D platform requirement~:P: ~{~A~^ ~}~@:>"
               (length requirements) requirements)
    (when (and platform installed-packages)
      (dolist (requirement requirements)
        (with-simple-restart
            (continue "~@<Ignore the requirement ~A.~@:>" requirement)
          (or (find requirement installed-packages
                    :test #'string= :key #'first)
              (error 'jenkins.analysis:unfulfilled-platform-dependency-error
                     :dependency requirement))))))
  distributions)
