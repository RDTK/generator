;;;; packaging.lisp ---
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.scripting)

(when nil
  (dolist (job (all-jobs))
    (dolist (builder (builders job))
      (when (and (typep builder 'builder/shell)
                 (ppcre:scan "CPACK_PACKAGE_REVISION.*=.*\"?-(b|r)\\$\\{BUILD_NUMBER\\}\"?" (command builder)))
        (ppcre:register-groups-bind (rev)
            ("CPACK_PACKAGE_REVISION.*=.*\"?-(b|r)(\\$\\{BUILD_NUMBER\\})\"?" (command builder))
          (when (find rev (environment job) :test #'search)
            (setf (environment job)
                  (cons (format nil "PACKAGE_REVISION=-~A${BUILD_NUMBER}"
                                (if (ppcre:scan "trunk" (id job))
                                    "b" "r"))
                        (remove-if (curry #'search "PACKAGE_REVISION")
                                   (environment job))))

            (setf (command builder)
                  (ppcre:regex-replace
                   "CPACK_PACKAGE_REVISION.*=.*\"?-(b|r)\\$\\{BUILD_NUMBER\\}\"?"
                   (command builder)
                   (format nil "CPACK_PACKAGE_REVISION=${PACKAGE_REVISION}")))

            (commit! job)))))))


(dolist (job (all-jobs "^rsc"))
  (dolist (builder (builders job))
    (when (and (typep builder 'builder/shell)
               (ppcre:scan "CPACK_PACKAGE_REVISION.*=[^ ]+" (command builder)))
      (unless (find "PACKAGE_REVISION.*=.*DEBIAN_DISTRIBUTION" (environment job) :test #'ppcre:scan)
        (setf (environment job)
              (cons (format nil "PACKAGE_REVISION=-~A${BUILD_NUMBER}~~${DEBIAN_DISTRIBUTION}"
                            (if (ppcre:scan "trunk" (id job))
                                "b" "r"))
                    (remove-if (curry #'search "PACKAGE_REVISION")
                               (environment job)))))
      (setf (command builder)
            (ppcre:regex-replace
             "CPACK_PACKAGE_REVISION.*= *[^ ]+"
             (command builder)
             (format nil "CPACK_PACKAGE_REVISION=${PACKAGE_REVISION}")))))
  (format t "~A~%~2@T~S~%~2@T~S~%"
          (id job)
          (environment job)
          (builders job))
  (commit! job))
