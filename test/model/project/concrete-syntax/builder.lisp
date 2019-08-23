;;;; builder.lisp --- Unit tests for the concrete syntax builder.
;;;;
;;;; Copyright (C) 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:build-generator.model.project.test)

(in-suite :build-generator.model.project.concrete-syntax)

(test protect-string.smoke
  "Smoke test for the `protect-string' function."

  (let ((raw "${foo} \\${bar} \\baz $fez \\\\"))
    (is (equal raw (build-generator.model.variables:value-parse
                    (build-generator.model.project::protect-string raw))))))
