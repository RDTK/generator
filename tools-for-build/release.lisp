#!/usr/bin/cl -s alexandria -s split-sequence -s let-plus -s inferior-shell -s eclector-concrete-syntax-tree

(cl:defpackage #:build-generator.tools-for-build.release
  (:use
   #:cl
   #:alexandria
   #:split-sequence
   #:let-plus)

  (:local-nicknames
   (#:shell #:inferior-shell)))

(cl:in-package #:build-generator.tools-for-build.release)

;;; Git actions

(defun branch (name)
  (shell:run `("git" "branch" ,name)))

(defun tag (name)
  (shell:run `("git" "tag" ,name)))

(defun checkout (name)
  (shell:run `("git" "checkout" ,name)))

(defun commit (message)
  (shell:run `("git" "commit" "-a" "-m" ,message)))

;;;

(defun read-version ()
  (let* ((current/string (uiop:read-file-form "version-string.sexp"))
         (current/list   (map 'list #'parse-integer
                              (split-sequence #\. current/string))))
    (assert (= 3 (length current/list)))
    (values current/list current/string)))

(defun bump-version (new-version)
  (with-output-to-file (stream "version-string.sexp" :if-exists :supersede)
    (format stream "\"~{~A~^.~}\"~%" new-version)))

(defun add-release-date ()
  (let+ ((file          "changes.sexp")
         (content       (read-file-into-string file))
         (cst           (with-input-from-string (stream content)
                          (eclector.concrete-syntax-tree:cst-read stream)))
         (version       (cst:second (cst:first cst)))
         (date          (cst:third (cst:first cst)))
         ((start . end) (cst:source date))
         ((&values &ign &ign &ign day month year)
          (decode-universal-time (get-universal-time))))
    (unless (null (cst:raw date))
      (error "~@<There already is a release date in the entry for release ~A.~@:>"
             version))
    (setf content (format nil "~A~
                               \"~D-~2,'0D-~2,'0D\"~
                               ~A"
                          (subseq content 0 start)
                          year month day
                          (subseq content end)))
    (write-string-into-file content file :if-exists :supersede)))

(defun add-release (version)
  (let+ ((file           "changes.sexp")
         (content        (read-file-into-string file))
         (cst            (with-input-from-string (stream content)
                           (eclector.concrete-syntax-tree:cst-read stream)))
         (release        (cst:first cst))
         ((start . &ign) (cst:source release)))
    (setf content (format nil "~A~
                               ~(~S~)~@
                               ~@
                               ~1@T~A"
                          (subseq content 0 start)
                          `(:release ,version nil)
                          (subseq content start)))
    (write-string-into-file content file :if-exists :supersede)))

(defun release ()
  (let+ ((this-version/list   (read-version))
         (next-version/list   (let+ (((major minor commit) this-version/list))
                                (list major (1+ minor) commit)))
         (this-release/string (format nil "~{~D~^.~}" (subseq this-version/list 0 2)))
         (next-release/string (format nil "~{~D~^.~}" (subseq next-version/list 0 2))))
    ;; Added release date to current section in changes.sexp.
    (add-release-date)
    (commit (format nil "Added date to ~A release in changes.sexp~@
                         ~@
                         * changes.sexp (release ~:*~A): added release date~%"
                    this-release/string))

    ;; Create release branch and release tag.
    (branch this-release/string)
    (tag (format nil "release-~A" this-release/string))

    ;; Bump version in version-string.sexp and create new section in
    ;; changes.sexp
    (bump-version next-version/list)
    (add-release next-release/string)
    (commit (format nil "Version bump ~A -> ~A in version-string.sexp~@
                         ~@
                         * version-string.sexp: changed value ~2:*\"~A.0\" -> \"~A.0\"~@
                         * changes.sexp (release ~:*~A): new section~%"
                    this-release/string next-release/string))))

(release)
