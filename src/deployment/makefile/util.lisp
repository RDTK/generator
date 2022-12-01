;;;; util.lisp --- Utilities used in the deployment.makefile module.
;;;;
;;;; Copyright (C) 2018-2022 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:build-generator.deployment.makefile)

;;; Escape dollars in shell fragments

(defun escape-dollars (string)
  (with-output-to-string (stream)
    (loop :for char :across string
          :when (char= char #\$)
            :do (write-char #\$ stream)
          :do (write-char char stream))))

;;; Work around problems with recipe lines starting with -, + or @
;;;
;;; One of these characters at the start of a recipe line causes make
;;; to interpret it and strip it from the recipe before passing the
;;; remainder to the shell. To avoid problems with, for example,
;;; patches (which contain lines starting with +, - and @) processed
;;; via here-documents, we detect recipes which contain such lines and
;;; switch to a completely different representation. We replace the
;;; recipe with one that contains the original recipe in a
;;; base64-encoded here-document which is decoded and then evaluated
;;; by the shell.

(defun forbidden-at-start-of-line? (char)
  (member char '(#\- #\+ #\@)))

(defun contains-problematic-line-beginning? (string)
  (loop :with at-line-beginning? = t
        :for char :across string
        :do (cond ((char= char #\Newline)
                   (setf at-line-beginning? t))
                  ((and at-line-beginning?
                        (forbidden-at-start-of-line? char))
                   (return t))
                  (t
                   (setf at-line-beginning? nil)))))

(defun base64-encode (string)
  (let ((variable   "commands")
        (eof-marker (format nil "EOF~16,'0X"
                            (ldb (byte (* 16 8) 0)
                                 (random most-positive-fixnum))))
        (blob       (cl-base64:string-to-base64-string string :columns 70)))
    (format nil "# The following blob contains shell commands that ~@
                 # cannot be expressed as make recipes (such as ~@
                 # here-documents containing lines starting with +, - or ~@
                 # @). The blob is decoded and passed to the shell ~@
                 # function eval.~@
                 OLD_IFS=\"${IFS}\"~@
                 export IFS=''~@
                 ~A=$(echo \"export IFS='${OLD_IFS}'\" && cat <<'~A' | base64 -d~@
                 ~A~@
                 ~2:*~A~@
                 )~@
                 eval ${~2:*~A}"
            variable eof-marker blob)))

(defun maybe-base64-encode (string)
  (if (contains-problematic-line-beginning? string)
      (base64-encode string)
      string))
