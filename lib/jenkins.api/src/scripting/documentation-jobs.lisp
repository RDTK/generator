(dolist (job (all-jobs))
  (restart-case
      (when-let* ((sh-steps          (remove-if-not (of-type 'builder/shell) (builders job)))
                  (interesting-steps (remove-if-not
                                      (lambda (builder) (ppcre:scan "scp" (command builder)))
                                      sh-steps)))
        (format t "~A64~%" job)

        (removef (builders job) (first interesting-steps))
        (commit! job)

        (let ((new-job (copy-job/fixup (id job) (format nil "~A-documentation" (id job)))))

          (setf (triggers new-job) '())
          (relate job new-job)

          (ppcre:register-groups-bind (path mid suffix) ("_rsa +([^ ]*) .*@cormine:/srv/doc/([^/]+)(/.*)"
                                                         (command (first interesting-steps)))
            (appendf
             (publishers new-job)
             (list
              (jenkins.dsl:ssh (:target           (if (string= mid "amarsi") "cormine" "cordocs")
                                :source-files     (format nil "~A/**/*" path)
                                :excludes         nil
                                :remove-prefix    path
                                :remote-directory (if (string= mid "amarsi")
                                                      (concatenate 'string mid suffix "/html")
                                                      (format nil "~A-api/trunk" (id job)))
                                :verbose?         nil)))))

          (commit! new-job)))
    (continue (&optional condition) (declare (ignore condition)))))
