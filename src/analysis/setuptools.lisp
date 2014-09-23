;;;; setuptools.lisp ---
;;;;
;;;; Copyright (C) 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.analysis)

(defparameter *global-variable-scanner*
  (ppcre:create-scanner #.(format nil "(?:~
  ^([a-z_][a-zA-Z0-9_]*)[ \\t\\n]*=[ \\t\\n]*\\[[ \\t\\n]*((?:[^]]|\\n)*)\\]~
  |~
  ^([a-z_][a-zA-Z0-9_]*)[ \\t\\n]*=[ \\t\\n]*\\{[ \\t\\n]*((?:[^}]|\\n)*)\\}~
  |~
  ([a-z_][a-zA-Z0-9_]*)[ \\t\\n]*=[ \\t\\n]*['\"]+((?:[^'\"]|\\n)*)['\"]+~
  |~
  ^([a-z_][a-zA-Z0-9_]*)[ \\t\\n]*=[ \\t\\n]*((?:[^,]|\\n)*)~
  )")
                        :multi-line-mode t))

(defparameter *define-project-version-scanner*
  (ppcre:create-scanner
   "^\\([ \\t\\n]*([a-z_][a-zA-Z0-9_]*)[ \\t\\n]*,[ \\t\\n]*([a-z_][a-zA-Z0-9_]*)[ \\t\\n]*\\)[ \\t\\n]*=[ \\t\\n]*defineProjectVersion\\([ \\t\\n]*['\"]([^'\"]+)['\"][ \\t\\n]*\\)$"
   :multi-line-mode t))

(defparameter *keyword-arg-scanner*
  (ppcre:create-scanner #.(format nil "(?:~
  ([a-z_][a-zA-Z0-9_]*)[ \\t\\n]*=[ \\t\\n]*\\[[ \\t\\n]*((?:[^]]|\\n)*)\\]~
  |~
  ([a-z_][a-zA-Z0-9_]*)[ \\t\\n]*=[ \\t\\n]*\\{[ \\t\\n]*((?:[^}]|\\n)*)\\}~
  |~
  ([a-z_][a-zA-Z0-9_]*)[ \\t\\n]*=[ \\t\\n]*['\"]+((?:[^'\"]|\\n)*)['\"]+~
  |~
  ([a-z_][a-zA-Z0-9_]*)[ \\t\\n]*=[ \\t\\n]*((?:[^,]|\\n)*)~
  )")
                        :multi-line-mode t)
  "TODO(jmoringe): document")

(defun extract-global-variables (source)
  (let ((result '()))
    (ppcre:do-register-groups (name1 value1 name2 value2 name3 value3 name4 value4)
        (*global-variable-scanner* source)
      (let+ ((name  (or name1 name2 name3 name4))
             (value (or value1 value2 value3 value4))
             ((&flet trim (string)
                (string-trim '(#\' #\" #\Newline #\Space) string)))
             (values (remove-if #'emptyp (mapcar #'trim (split-sequence #\, value)))))
        (when values
          (push (cons name values) result))))
    (ppcre:register-groups-bind (version commit value)
        (*define-project-version-scanner* source)
      (declare (ignore commit))
      (push (cons version (list value)) result))
    result))

(defun extract-keyword-arguments (source)
  (let ((result '()))
    (ppcre:do-register-groups (name1 value1 name2 value2 name3 value3 name4 value4)
        (*keyword-arg-scanner* source)
      (let+ ((name  (or name1 name2 name3 name4))
             (value (or value1 value2 value3 value4))
             ((&flet trim (string)
                (string-trim '(#\' #\" #\Newline #\Space) string)))
             (values (remove-if #'emptyp (mapcar #'trim (split-sequence #\, value)))))
        (push (cons name values) result)))
    result))

(defun process-version (spec globals)
  (if-let ((values (when (stringp spec)
                    (find spec globals :test #'string= :key #'car))))
    (if (string= spec (first (cdr values)))
        spec
        (process-version (first (cdr values)) globals))
    (parse-version spec)))

(defun process-dependency (spec globals)
  (or (ppcre:register-groups-bind (name relation version)
          ("^([^ \\t<>=]+)[ \\t]*([<>=]+)[ \\t]*([^ \\t]+)$" spec)
        (list* :setuptools name
               (when (string= relation ">=")
                 (list (process-version version globals)))))
      (list :setuptools spec)))

(defmethod analyze ((directory pathname)
                    (kind      (eql :setuptools))
                    &key)
  (let+ ((content    (read-file-into-string* (merge-pathnames "setup.py" directory)))
         (globals    (extract-global-variables content))
         (setup-call (ppcre:scan-to-strings "setup\\((?:.|\\n)*\\)" content))
         (arguments  (extract-keyword-arguments setup-call))
         ((&flet argument (name &optional list?)
            (let ((cell (cdr (assoc name arguments :test #'string=))))
              (if list? cell (first cell)))))
         (name        (argument "name"))
         (version     (process-version (argument "version") globals))
         (description (argument "description"))
         (keywords    (when-let ((keywords (argument "keywords" t)))
                        (typecase keywords
                          (string (split-sequence-if
                                   (rcurry #'member '(#\Space #\Tab))
                                   keywords))
                          (t      keywords ))))
         (url         (argument "url"))
         ((&flet format-person (name email)
            (format nil "~A ~@[<~A>~]" name email)))
         ((&flet split-persons (string)
            (ppcre:split "[ \\t]*(?:,|and)[ \\t]*" string)))
         (authors     (when-let ((author (argument "author")))
                        (split-persons author)))
         (emails      (when-let ((email (argument "author_email")))
                        (split-persons email)))
         (authors     (mapcar #'format-person
                              authors (or emails (circular-list nil))))
         (license     (or (argument "license")
                          (analyze directory :license)))
         (requires    (mapcar (rcurry #'process-dependency globals)
                              (append
                               (argument "setup_requires" t)
                               (argument "install_requires" t)))))
    (append
     (list :versions `((:main . ,version))
           :provides `((:setuptools ,name ,version))
           :requires requires)
     (when description
       `(:description ,description))
     (when keywords
       `(:keywords ,keywords))
     (when url
       `(:url ,url))
     (when authors
       `(:authors ,authors))
     (when license
       `(:license ,license)))))
