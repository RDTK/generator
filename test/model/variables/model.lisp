;;;; model.lisp --- Unit tests for variable evaluation.
;;;;
;;;; Copyright (C) 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.model.variables.test)

(in-suite :jenkins.project.model.variables)

(test value-unparse.smoke
  "Smoke test for the `value-unparse' function."

  (mapc (lambda+ ((parsed expected))
          (is (equalp expected (value-unparse parsed))))

        `((,(value-parse #1=t)                                      #1#)
          (,(value-parse #2=nil)                                    #2#)
          (,(value-parse #3=1)                                      #3#)

          (,(value-parse #4="foo")                                  #4#)
          (,(value-parse #5="foo\\${foo}")                          #5#)

          (,(value-parse #6="${bar}")                               #6#)
          (,(value-parse #7="${bar|default}")                       #7#)
          (,(value-parse #8="@{bar}")                               #8#)
          (,(value-parse #9="@{bar|default}")                       #9#)

          (,(value-parse '#10=(1 "foo" "@{bar|default}"))           #10#)

          (,(value-parse '#11=((:foo . 1) (:bar "@{bar|default}"))) #11#))))
