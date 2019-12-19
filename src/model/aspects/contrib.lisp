;;;; contrib.lisp --- Obsolete or currently unused aspects.
;;;;
;;;; Copyright (C) 2012-2017, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:build-generator.model.aspects)

;;; Win32-related aspects

(define-aspect dependency-download/win32 (builder-defining-mixin) ()
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
  (with-interface (jenkins.api:publishers job) (warnings (jenkins.api:publisher/warnings))
    (pushnew (make-instance 'jenkins.api:warning-parser/console :name "Lintian")
             (jenkins.api:console-parsers warnings)
             :test #'string=
             :key  #'name))

  ;; Archive the generated Debian package.
  (with-interface (jenkins.api:publishers job)
      (archiver (jenkins.api:publisher/archive-artifacts
                 :files        nil
                 :only-latest? nil))
    (pushnew #?"${(var/typed :build-dir 'string)}/*.deb" (files archiver)
             :test #'string=)))

(define-aspect (debian-package/cmake) (debian-package
                                       builder-defining-mixin)
    ()
  (push (constraint! (build ((:after cmake/unix)))
                     (shell (:command #?"mkdir -p ${(var/typed :build-dir 'string)} && cd ${(var/typed :build-dir 'string)}
cmake -DCPACK_CONFIG_FILE=${(var/typed :aspect.debian-package/cmake.cpack-config-file 'string)} \\
      -DCPACK_PACKAGE_REVISION=\${PACKAGE_REVISION} \\
      ..
umask 022
\${FAKEROOT_FOR_CPACK} make package
lintian -i *.deb || true
")))
        (jenkins.api:builders job)))
