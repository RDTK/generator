;;;; git.lisp ---
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.analysis)

(defmethod analyze ((source puri:uri) (schema (eql :git))
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
  "TODO(jmoringe): document"
  ;;; TODO(jmoringe, 2013-01-17): find branches automatically
  (let* ((project-name      (lastcar (puri:uri-parsed-path source)))
         (repository/string (format-git-url source username password))
         (clone-directory   (merge-pathnames
                             (make-pathname :directory (list :relative project-name))
                             temp-directory))
         (analyze-directory (if sub-directory
                                (merge-pathnames sub-directory clone-directory)
                                clone-directory)))

    (with-sequence-progress (:analyze/branch branches)
      (inferior-shell:run/lines
       `(inferior-shell:&host (inferior-shell:local ,temp-directory)
          (git "clone" :quiet ,repository/string ,clone-directory)))

      (unwind-protect
           (iter (for branch in (append branches tags))
                 (progress "~A" branch)

                 (restart-case
                     (progn
                       (inferior-shell:run/lines
                        `(inferior-shell:&host (inferior-shell:local ,clone-directory)
                           (git :work-tree ,clone-directory
                                :git-dir ,(merge-pathnames ".git/" clone-directory)
                                "checkout" :quiet ,branch)))

                       (let ((result (list* :scm              :git
                                            :branch-directory nil
                                            (analyze analyze-directory :auto))))
                         (unless (getf result :authors)
                           (setf (getf result :authors)
                                 (analyze clone-directory :git/authors)))
                         (collect (cons branch result))))
                   (continue (&optional condition)
                     :report (lambda (stream)
                               (format stream "~<Ignore ~A and ~
                                               continue with the next ~
                                               branch.~@:>"
                                       branch))
                     (declare (ignore condition)))))

        (inferior-shell:run/lines
         `(inferior-shell:&host (inferior-shell:local ,temp-directory)
            (rm "-rf" ,clone-directory)))))))

(defmethod analyze ((directory pathname) (kind (eql :git/authors))
                    &key
                    (max-authors 5))
  (with-trivial-progress (:analyze/log "~A" directory)
    (let* ((lines
             (inferior-shell:run/lines
              `(inferior-shell:&host (inferior-shell:local ,directory)
                                     (git :no-pager ,(format nil "--git-dir=~A/.git" directory)
                                          "log" "--pretty=format:%an <%ae>"))))
           (frequencies (make-hash-table :test #'equal)))
      (dolist (line lines)
        (incf (gethash line frequencies 0)))
      (setf frequencies (sort (hash-table-alist frequencies) #'> :key #'cdr))
      (mapcar #'car (subseq frequencies 0 (min (length frequencies) max-authors))))))
