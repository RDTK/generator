;;;; find-rsc.lisp ---
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.scripting)

(defun find-rsc ()
  (dolist (job (all-jobs))
    (let ((copy)
          (shell))
      (dolist (builder (builders job))
        (when (and (typep builder 'builder/copy-artifact)
                   (ppcre:scan "rsc.*trunk" (project-name builder)))
          (setf copy builder))
        (when (and (typep builder 'builder/shell)
                   (ppcre:scan "RSC=" (command builder))
                   (ppcre:scan "RSC_DIR=.*\\${RSC}" (command builder)))
          (setf shell builder)))
      (when (and copy shell)
        (let ((old-find (ppcre:scan-to-strings "RSC=.*" (command shell)))
              (old-rsc  (ppcre:scan-to-strings "RSC_DIR=[^ ]*" (command shell))))
          (setf (command shell)
                (ppcre:regex-replace
                 "RSC=.*"
                 (command shell)
                 "RSC=\"$(find \"${WORKSPACE}/upstream\" -type f -name \"RSCConfig.cmake\" -exec dirname {} \\;)\""))
          (setf (command shell)
                (ppcre:regex-replace
                 "RSC_DIR=[^ ]*" (command shell) "RSC_DIR=\"${RSC}\""))
          (format t "~A~%~2@T~A~%~2@T~A~%->~A~%~2@T~A~%->~A~%"
                  (id job)
                  (project-name copy)
                  old-find
                  (ppcre:scan-to-strings "RSC.*=.*" (command shell))
                  old-rsc
                  (ppcre:scan-to-strings "RSC_DIR=[^ ]*" (command shell))))))))

(defun find-nemomath ()
  "TODO(jmoringe): document"
  (dolist (job (all-jobs))
    (unless (or (string= (id job) "m3s Static Analysis")
                (string= (id job) "Template CMake Multiconf")
                (string= (id job) "Template GradSchool Multiconf"))
     (let ((changed? nil)
           (builder1))
       (dolist (builder (builders job))
         (when (and (typep builder 'builder/shell)
                    (or (ppcre:scan "NEMOMATH=`find \"\\${WORKSPACE}/upstream\" -maxdepth 1 -type d -name \"NemoMath\\*\";`"
                                    (command builder))
                        (ppcre:scan "-DNemoMath_DIR=${NEMOMATH}/share/NemoMath"
                                    (command builder))))
           (setf builder1 builder)
           (setf (command builder)
                 (ppcre:regex-replace-all
                  "NEMOMATH=`find \"\\${WORKSPACE}/upstream\" -maxdepth 1 -type d -name \"NemoMath\\*\";`"
                  (command builder)
                  "NEMOMATH=\"$(find \"${WORKSPACE}/upstream\" -type f -name \"NemoMathConfig.cmake\" -exec dirname {} \\;)"))
           (setf (command builder)
                 (ppcre:regex-replace-all
                  "-DNemoMath_DIR=\\${NEMOMATH}/share/NemoMath"
                  (command builder)
                  "-DNemoMath_DIR=\"${NEMOMATH}\""))
           (setf changed? t)))

       (when changed?
         (format t "~A~%~2@T~<~@;~A~:>~%~2@T~<~@;~A~:>~%"
                 (id job)
                 (list (ppcre:scan-to-strings "NEMOMATH=.*" (command builder1)))
                 (list (ppcre:scan-to-strings "-DNemoMath_DIR=.*" (command builder1))))
         (commit! job))))))
