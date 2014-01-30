(dolist (node (all-nodes "ubuntu"))
  (setf (getf (environment node) :DEBIAN_DISTRIBUTION)
        (elt (nth-value 1 (ppcre:scan-to-strings "ubuntu_([^_])+" (id node))) 0))
  (format t "~A: ~S~%" (id node) (environment node)))

(dolist (job (all-jobs))
  (when (find "PACKAGE_REVISION" (environment job)
              :test #'search)
    (setf (environment job)
          (cons (format nil "~A~~${DEBIAN_DISTRIBUTION}"
                        (find "PACKAGE_REVISION" (environment job)
                              :test #'search))
                (remove "PACKAGE_REVISION" (environment job)
                        :test #'search)))
    (format t "~A: ~S~%"
            (id job)
            (find "PACKAGE_REVISION" (environment job)
                  :test #'search))))
