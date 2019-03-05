;;;; setuptools.lisp ---
;;;;
;;;; Copyright (C) 2013-2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.analysis)

(defparameter *value-scanner*
  (ppcre:create-scanner #. (format nil "~
    (?:~
      \\[~
        [ \\t\\n]*((?:[^]]|\\n)*)~
      \\]~
     |~
      \\{~
        [ \\t\\n]*((?:[^}]|\\n)*)~
      \\}~
     |~
      (?:'|''')((?:[^']|\\n)*)(?:'|''')~
     |~
      (?:\"|\"\"\")((?:[^\"]|\\n)*)(?:\"|\"\"\")~
     |~
      ((?:[^,]|\\n)*)~
    )")))

(defparameter *global-variable-scanner*
  (ppcre:create-scanner #.(format nil "~
    ^([a-z_][a-zA-Z0-9_]*)[ \\t\\n]*=[ \\t\\n]*~
    (~
      \\[~
        [ \\t\\n]*(?:[^]]|\\n)*~
      \\]~
     |~
      \\{~
        [ \\t\\n]*(?:[^}]|\\n)*~
      \\}~
     |~
      (?:'|''')(?:[^']|\\n)*(?:'|''')~
     |~
      (?:\"|\"\"\")(?:[^\"]|\\n)*(?:\"|\"\"\")~
     |~
      .+~
    )")
                        :multi-line-mode t))

(defparameter *define-project-version-scanner*
  (ppcre:create-scanner
   "^\\([ \\t\\n]*([a-z_][a-zA-Z0-9_]*)[ \\t\\n]*,[ \\t\\n]*([a-z_][a-zA-Z0-9_]*)[ \\t\\n]*\\)[ \\t\\n]*=[ \\t\\n]*defineProjectVersion\\([ \\t\\n]*['\"]([^'\"]+)['\"][ \\t\\n]*\\)$"
   :multi-line-mode t))

(defparameter *keyword-arg-scanner*
  (ppcre:create-scanner #.(format nil "~
    ([a-z_][a-zA-Z0-9_]*)[ \\t\\n]*=[ \\t\\n]*~
    (~
      \\[~
        [ \\t\\n]*(?:[^]]|\\n)*~
      \\]~
     |~
      \\{~
        [ \\t\\n]*(?:[^}]|\\n)*~
      \\}~
     |~
      (?:'|''')(?:[^']|\\n)*(?:'|''')~
     |~
      (?:\"|\"\"\")(?:[^\"]|\\n)*(?:\"|\"\"\")~
     |~
      (?:[^,]|\\n)*~
    )")
                        :multi-line-mode t))

(defun extract-value (source)
  (labels
      ((strip (string)
         (string-trim '(#\Newline #\Space) string))
       (scan ()
         (ppcre:do-register-groups (value1 value2 value3 value4 value5)
             (*value-scanner* source)
           (return-from scan
             (cond
               (value1
                (mapcan (compose #'extract-value #'strip)
                        (split-sequence #\, value1)))
               (value2
                (mapcan (compose #'extract-value #'strip)
                        (split-sequence-if
                         (lambda (character)
                           (member character '(#\, #\:)))
                         value2)))
               (value3
                (list (string-trim '(#\') value3)))
               (value4
                (list (string-trim '(#\") value4)))
               (value5
                (list (strip value5))))))))
    (remove-if #'emptyp (scan))))

(defun extract-global-variables (source)
  (let ((result '()))
    (ppcre:do-register-groups (name value)
        (*global-variable-scanner* source)
      (when-let ((values (extract-value value)))
        (push (cons name values) result)))
    (ppcre:register-groups-bind (version commit value)
        (*define-project-version-scanner* source)
      (declare (ignore commit))
      (push (cons version (list value)) result))
    result))

(defun extract-keyword-arguments (source)
  (let ((result '()))
    (ppcre:do-register-groups (name value)
        (*keyword-arg-scanner* source)
      (when-let ((values (extract-value value)))
        (push (cons name values) result)))
    result))

(defun process-version (spec globals)
  (if-let ((values (when (stringp spec)
                     (find spec globals :test #'string= :key #'car))))
    (if (string= spec (first (cdr values)))
        spec
        (process-version (first (cdr values)) globals))
    (when (ppcre:scan "^[-_.:0-9a-zA-Z]+$" spec)
      (parse-version spec))))

(defun process-dependency (spec globals)
  (or (ppcre:register-groups-bind (name relation version)
          ("^([^ \\t\"'{}<>=]+)[ \\t]*([<>=]+)[ \\t]*([^ \\t]+)$" spec)
        (list (list* :setuptools name
                     (cond
                       ((not (string= relation ">="))
                        nil)
                       ((when-let (version (process-version version globals))
                          (list version)))))))
      (ppcre:register-groups-bind (name) ("^([^ \\t\"'{}<>=]+)" spec)
        (list (list :setuptools name)))))

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
         (authors     (parse-name-and-email
                       (argument "author") (argument "author_email")))
         (license     (argument "license"))
         (requires    (mapcan (rcurry #'process-dependency globals)
                              (append (argument "setup_requires" t)
                                      (argument "install_requires" t)))))
    `(:natures               (,kind)
      :provides              ((:setuptools ,name ,version))
      :requires              ,requires
      :programming-languages ("Python")
      ,@(when description `(:description ,description))
      ,@(when keywords    `(:keywords    ,keywords))
      ,@(when url         `(:url         ,url))
      ,@(when authors     `(:authors     ,authors))
      ,@(when license     `(:license     ,license)))))

;;; Utilities

(defun parse-name-and-email (name-or-names email-or-emails)
  (let+ ((names         (when name-or-names
                          (parse-people-list name-or-names)))
         (names-length  (length names))
         (emails        (when email-or-emails
                          (parse-people-list email-or-emails)))
         (emails-length (length emails))
         (max-length    (max names-length emails-length))
         ((&flet pad (list length)
            (if (< length max-length)
                (append list (make-list (- max-length length)))
                list))))
    (merge-people-list
     (map 'list (lambda (person email)
                  (if (and person email)
                      (rosetta-project.model.resource:augment-person!
                       person (rosetta-project.model.resource:identities email))
                      (or person email)))
          (pad names names-length) (pad emails emails-length)))))
