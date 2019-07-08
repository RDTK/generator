;;;; types.lisp --- Types used in the commandline-options module.
;;;;
;;;; Copyright (C) 2017, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:build-generator.commandline-options)

(deftype positional-option-designator ()
  '(or non-negative-integer (eql &rest)))

(defun named-option-designator? (thing)
  (and (stringp thing) (starts-with #\- thing)))

(deftype named-option-designator ()
  `(and string (satisfies named-option-designator?)))

(deftype option-designator ()
  `(or positional-option-designator named-option-designator))
