;;;; package.lisp --- Package definition for the analysis module.
;;;;
;;;; Copyright (C) 2012, 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.analysis)

;;; Current platform

#+linux
(defun lsb-distribution ()
  (inferior-shell:run/ss '("lsb_release" "-si")))

#+linux
(defun current-platform ()
  (values (string-downcase (lsb-distribution))
          (inferior-shell:run/ss '("lsb_release" "-sc"))
          (inferior-shell:run/ss '("uname" "-m"))))

;;; Installed packages

(defun installed-packages/not-implemented (type &key (signal-via #'error))
  (funcall signal-via "~@<Don't know how to find installed packages on ~
                       this system (of type ~{~A~^ ~})~@:>"
           (ensure-list type)))

;; Linux

;; lsb_release distribution => { Debian, Ubuntu }
(defun installed-packages/linux/debian-like ()
  (let ((lines (inferior-shell:run/lines
                '("dpkg-query"
                  "--showformat=${Package} ${Version} ${Status}\\n"
                  "--show"))))
    (iter (for line in lines)
          (when (ends-with-subseq "install ok installed" line)
            (let+ (((name version)
                    (subseq (split-sequence #\Space line) 0 2)))
              (collect (list name (parse-version version))))))))

;; lsb_release distribution => Arch
(defun installed-packages/linux/arch ()
  (let ((lines (inferior-shell:run/lines '("pacman" "-Q"))))
    (iter (for line in lines)
          (let+ (((name version) (split-sequence #\Space line)))
            (collect (list name (parse-version version)))))))

(defvar *installed-packages-functions/linux*
  `(("Ubuntu" . installed-packages/linux/debian-like)
    ("Debian" . installed-packages/linux/debian-like)
    ("Arch"   . installed-packages/linux/arch)))

(defun installed-packages/linux ()
  (when-let* ((distribution (lsb-distribution))
              (function     (or (cdr (assoc distribution *installed-packages-functions/linux*
                                            :test #'string=))
                                (curry #'installed-packages/not-implemented
                                       (list distribution "Linux")))))
    (log:info "~@<This seems to be a ~A Linux system~@:>" distribution)
    (funcall function)))

(defvar *installed-packages-functions*
  `(("Linux" . installed-packages/linux)))

(defun installed-packages ()
  (with-simple-restart (continue "~@<Continue without checking ~
                                  installed packages.~@:>")
    (when-let* ((type     (software-type))
                (function (or (cdr (assoc type *installed-packages-functions*
                                          :test #'string=))
                              (curry #'installed-packages/not-implemented
                                     type :signal-via #'warn))))
      (log:info "~@<This seems to be a ~A system~@:>" type)
      (funcall function))))
