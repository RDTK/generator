;;;; builder.lisp --- Unit tests for the concrete syntax builder.
;;;;
;;;; Copyright (C) 2019, 2020 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:build-generator.model.project.test)

(in-suite :build-generator.model.project.concrete-syntax)

(test expand-pathname.smoke
  "Smoke test for the `expand-pathname' function."

  (mapc
   (lambda+ ((base-path root-path pathname expected))
     (let* ((builder (make-instance 'build-generator.model.project::recipe-builder
                                    :base-path base-path :root-path root-path))
            (result  (build-generator.model.project::expand-pathname
                      builder pathname)))
       (is (equalp expected result))))
   '((#P"/directory/name.type" #P"/root/" "foo"           #P"/directory/foo")
     (#P"/directory/name.type" #P"/root/" "foo.bar"       #P"/directory/foo.bar")
     (#P"/directory/name.type" #P"/root/" "sub/foo"       #P"/directory/sub/foo")
     (#P"/directory/name.type" #P"/root/" "sub/foo.bar"   #P"/directory/sub/foo.bar")

     (#P"/directory/name.type" #P"/root/" "/foo"          #P"/foo")
     (#P"/directory/name.type" #P"/root/" "/foo.bar"      #P"/foo.bar")
     (#P"/directory/name.type" #P"/root/" "/sub/foo"      #P"/sub/foo")
     (#P"/directory/name.type" #P"/root/" "/sub/foo.bar"  #P"/sub/foo.bar")

     (#P"/directory/name.type" #P"/root/" "//foo"         #P"/root/foo")
     (#P"/directory/name.type" #P"/root/" "//foo.bar"     #P"/root/foo.bar")
     (#P"/directory/name.type" #P"/root/" "//sub/foo"     #P"/root/sub/foo")
     (#P"/directory/name.type" #P"/root/" "//sub/foo.bar" #P"/root/sub/foo.bar")

     (#P"/directory/name"      #P"/root/" "foo"           #P"/directory/foo")
     (#P"/directory/name"      #P"/root/" "foo.bar"       #P"/directory/foo.bar")
     (#P"/directory/name"      #P"/root/" "sub/foo"       #P"/directory/sub/foo")
     (#P"/directory/name"      #P"/root/" "sub/foo.bar"   #P"/directory/sub/foo.bar")

     (#P"/directory/name"      #P"/root/" "/foo"          #P"/foo")
     (#P"/directory/name"      #P"/root/" "/foo.bar"      #P"/foo.bar")
     (#P"/directory/name"      #P"/root/" "/sub/foo"      #P"/sub/foo")
     (#P"/directory/name"      #P"/root/" "/sub/foo.bar"  #P"/sub/foo.bar")

     (#P"/directory/name"      #P"/root/" "//foo"         #P"/root/foo")
     (#P"/directory/name"      #P"/root/" "//foo.bar"     #P"/root/foo.bar")
     (#P"/directory/name"      #P"/root/" "//sub/foo"     #P"/root/sub/foo")
     (#P"/directory/name"      #P"/root/" "//sub/foo.bar" #P"/root/sub/foo.bar"))))

(test protect-string.smoke
  "Smoke test for the `protect-string' function."

  (let ((raw "${foo} \\${bar} \\baz $fez \\\\"))
    (is (equal raw (build-generator.model.variables:value-parse
                    (build-generator.model.project::protect-string raw))))))
