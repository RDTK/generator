;;;; contrib.lisp --- Obsolete or currently unused aspects.
;;;;
;;;; Copyright (C) 2012-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.model.aspects)

;;; Win32-related aspects

(define-aspect dependency-download/win32 (builder-defining-mixin) ()
  ;; TODO share code for determining artifacts to copy with UNIX
  ;; dependency download
  (puse (constraint! ()
          (batch (:command "cd upstream
unzip -o *.zip
move *.zip ..
move RSC* RSC
move RSBProtocol* RSBProtocol
move ..\*.zip .")))
        (builders job)))

;;; Debian packaging-related aspects

(define-aspect (debian-package :job-var job) () ()
  ;; Add console-based parser for lintian.
  (with-interface (publishers job) (warnings (publisher/warnings))
    (pushnew (make-instance 'warning-parser/console :name "Lintian")
             (console-parsers warnings)
             :test #'string=
             :key  #'name))

  ;; Archive the generated Debian package.
  (with-interface (publishers job) (archiver (publisher/archive-artifacts
                                              :files        nil
                                              :only-latest? nil))
    (pushnew #?"${(var/typed :build-dir 'string)}/*.deb" (files archiver)
             :test #'string=)))

(define-aspect (debian-package/cmake) (debian-package
                                       builder-defining-mixin)
    ()
  ;; TODO add PACKAGE_REVISION to environment
  (push (constraint! (build ((:after cmake/unix)))
                     (shell (:command #?"mkdir -p ${(var/typed :build-dir 'string)} && cd ${(var/typed :build-dir 'string)}
cmake -DCPACK_CONFIG_FILE=${(var/typed :aspect.debian-package/cmake.cpack-config-file 'string)} \\
      -DCPACK_PACKAGE_REVISION=\${PACKAGE_REVISION} \\
      ..
umask 022
\${FAKEROOT_FOR_CPACK} make package
lintian -i *.deb || true
")))
        (builders job)))
