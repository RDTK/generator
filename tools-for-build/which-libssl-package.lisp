(ql:quickload '(:cl+ssl :split-sequence :inferior-shell))

(defun libssl-pathname ()
  (let ((libssl (find "libssl" (cffi:list-foreign-libraries)
                      :key  (alexandria:compose #'string-downcase #'cffi:foreign-library-name)
                      :test #'search)))
    (cffi:foreign-library-pathname libssl)))

(defun restrict-to-architecture (packages)
  (let ((architecture (inferior-shell:run '("dpkg" "--print-architecture")
                                           :output '(:string :stripped t))))
    (remove architecture packages
            :key #'second :test-not #'string=)))

(defun libssl-package ()
  (let* ((pathname (libssl-pathname))
         (lines    (inferior-shell:run
                    `("dpkg" "-S" ,(format nil "*/~A" pathname))
                    :output '(:lines :stripped t) :error-output *error-output*))
         (parsed   (mapcar (lambda (line)
                             (split-sequence:split-sequence #\: line))
                           lines))
         (matches  (restrict-to-architecture parsed))
         (package  (first (first matches))))
    (unless (alexandria:length= 1 matches)
      (warn "~@<Multiple packages contain ~S:~@:_~
             ~{* ~A~^~:@_~}~@:>"
            pathname matches))
    package))
