;;;; version.lisp --- Simple version model.
;;;;
;;;; Copyright (C) 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.analysis)

(defun parse-version (string)
  "Example:

     (parse-version \"0.9-ALPHA\")"
  (mapcar (lambda (component)
            (let+ (((&values number consumed)
                    (parse-integer component :junk-allowed t)))
              (if (length= consumed component)
                  number
                  component)))
          (split-sequence-if (rcurry #'member '(#\- #\.)) string
                             :remove-empty-subseqs t)))

(defun print-version (stream version &optional colon? at?)
  "Example:

     (format nil \"~/jenkins.analysis:print-version/\"
             '(9 2 3 \"APLPHA\"))"
  (declare (ignore colon? at?))
  (iter (for component in version)
        (unless (first-iteration-p)
          (write-char (etypecase component
                        (real   #\.)
                        (string #\-))
                      stream))
        (princ component stream)))

(defun version-component-< (left right)
  (let+ (((&flet ensure-string (thing)
            (etypecase thing
              (real   (princ-to-string thing))
              (string thing)))))
    (or (and (realp left) (realp right) (< left right))
        (string< (ensure-string left) (ensure-string right)))))

(defun version= (left right)
  (equal left right))

(defun version< (left right)
  "Example:

     (version< '(\"0\" \"8\" \"alpha\") '(\"0\" \"7\" \"beta\"))"
  (cond
    ((null left)  t)
    ((null right) nil)
    ((some #'version-component-< left right))
    ((and (every #'equal left right)
          (< (length left) (length right))))))

(defun version>= (left right)
  (not (version< left right)))

(defun version-matches (query version)
  "Example:

     (version-matches '(0 8 \"ALPHA\") '(0 8 1))"
  (not (version< version query)))
