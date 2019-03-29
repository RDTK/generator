;;;; describe.lisp --- Generate textual description of model objects..
;;;;
;;;; Copyright (C) 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:build-generator.language-server)

(defun describe-project (project)
  (let+ (((&flet attribute (name default)
            (handler-case
                (var:value/cast project name default)
              (error (condition)
                (format nil "Error: ~A" condition)))))
         (natures               (attribute :natures '()))
         (programming-languages (attribute :programming-languages '()))
         (licenses              (attribute :licenses '()))
         (maintainers           (ensure-list
                                 (attribute :recipe.maintainer '())))
         (description           (attribute :description nil)))
    (format nil "# Project ~A
                 ~@
                 **Nature~P**: ~:*~[*none known*~*~:;~{~A~^ ~}~]~%~
                 **Programming Language~P**: ~:*~[*none known*~*~:;~{~A~^ ~}~]~%~
                 **License~P**: ~:*~[*none known*~*~:;~{~A~^ ~}~]~%~
                 **Recipe Maintainer~P**:~
                 ~:*~[ *none known*~*~%~
                 ~:;~
                   ~%~
                   ~{  * ~A~^~%~}~
                 ~]~
                 ~%~:[*no description available*~;~:*~A~]"
            (model:name project)
            (length natures)               natures
            (length programming-languages) programming-languages
            (length licenses)              licenses
            (length maintainers)           maintainers
            description)))
