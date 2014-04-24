;;;; subversion.lisp --- Analyze subversion repositories.
;;;;
;;;; Copyright (C) 2012, 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.analysis)

(defun %svn-and-global-options (&optional username password)
  `(svn :non-interactive :quiet
        ,@(when username `(:username ,username))
        ,@(when password `(:password ,password))))

(defmethod analyze ((source puri:uri) (schema (eql :svn))
                    &key
                    username
                    password
                    branches
                    tags
                    sub-directory
                    (temp-directory (sb-ext:parse-native-namestring
                                     (sb-posix:mkdtemp "/tmp/project.XXXXXX")
                                     nil *default-pathname-defaults*
                                     :as-directory t)))
  (let+ ((project-name    (lastcar (puri:uri-parsed-path source)))
         (clone-directory (merge-pathnames
                           (make-pathname :directory (list :relative project-name))
                           temp-directory)))

    (with-trivial-progress (:checkout "~A" source)
      (inferior-shell:run
       `(,@(%svn-and-global-options username password)
         "co" ,(princ-to-string source) ,clone-directory)
       :directory temp-directory))

    (unwind-protect
         (let+ (((&flet find-branches (directory &optional pattern)
                   (let ((pattern (format nil "~A/~:[*/~;~:*~A~]" directory pattern)))
                     (mapcar
                      (compose #'lastcar #'pathname-directory)
                      (remove-if
                       (lambda (candidate)
                         (or (pathname-name candidate)
                             (string= ".svn" (lastcar (pathname-directory candidate)))))
                       (directory (merge-pathnames pattern clone-directory)))))))
                ((&flet make-branch-list (directory spec)
                   (mapcar (lambda (name)
                             (if (and (string= directory "branches")
                                      (string= name "trunk"))
                                 (list "trunk" "trunk/")
                                 (list name (format nil "~A/~A/" directory name))))
                           (etypecase spec
                             (null   (if (string= directory "branches")
                                         (cons "trunk" (find-branches directory))
                                         (find-branches directory)))
                             (string (find-branches directory spec))
                             (t      spec)))))
                (locations (append
                            (make-branch-list "branches" branches)
                            (make-branch-list "tags" tags))))

           (with-sequence-progress (:analyze/branch locations)
             (iter (for (name directory) in locations)
                   (progress "~A" name)
                   (restart-case
                       (let* ((directory/absolute (reduce #'merge-pathnames
                                                          (append
                                                           (when sub-directory
                                                             (list sub-directory))
                                                           (list
                                                            (parse-namestring directory)
                                                            clone-directory))))
                              (result
                                (list* :scm              :svn
                                       :branch-directory directory
                                       (analyze directory/absolute :auto))))
                         (unless (getf result :authors)
                           (setf (getf result :authors)
                                 (analyze (parse-namestring directory/absolute) :svn/authors
                                          :username username
                                          :password password)))
                         (collect (cons name result)))
                     (continue (&optional condition)
                       :report (lambda (stream)
                                 (format stream "~<Ignore ~A and ~
                                                 continue with the ~
                                                 next branch.~@:>"
                                         name))
                       (declare (ignore condition)))))))

      (inferior-shell:run `(rm "-rf" ,clone-directory)
                          :directory temp-directory))))

(defmethod analyze ((directory pathname) (kind (eql :svn/authors))
                    &key
                    username
                    password
                    (max-authors 5))
  (with-trivial-progress (:analyze/log "~A" directory)
    (let* ((lines
             (inferior-shell:run/lines
              `(,@(%svn-and-global-options username password)
                "log" ,directory)
              :directory directory))
           (frequencies (make-hash-table :test #'equal)))
      (dolist (line lines)
        (ppcre:register-groups-bind (author) ("r[0-9]+ \\| ([^|]+) \\|" line)
          (incf (gethash author frequencies 0))))
      (setf frequencies (sort (hash-table-alist frequencies) #'> :key #'cdr))
      (mapcar #'car (subseq frequencies 0 (min (length frequencies) max-authors))))))
