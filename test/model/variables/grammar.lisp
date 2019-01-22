;;;; grammar.lisp --- Unit tests for the variable grammar.
;;;;
;;;; Copyright (C) 2012-2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.model.variables.test)

(in-suite :jenkins.project.model.variables)

(test parse.smoke
  "Smoke test for parsing expressions of the variable language."

  (mapc
   (lambda+ ((input expected))
     (flet ((do-it ()
              (esrap:parse 'jenkins.model.variables::expr input)))
       (case expected
         (error (signals esrap:esrap-parse-error (do-it)))
         (t     (is (equal expected (do-it)))))))

   '((""                    nil)
     ("foo"                 ("foo"))
     ("foo$bar"             ("foo$bar"))
     ("foo{bar"             ("foo{bar"))
     ("foo}bar"             ("foo}bar"))
     ("foo\\${"             ("foo${"))
     ("foo\\@{"             ("foo@{"))

     ("${a"                 error)
     ("${a}"                ((:ref ("a"))))
     ("${a|}"               ((:ref ("a") :default nil)))
     ("${a|b}"              ((:ref ("a") :default "b")))
     ("${a|{b}}"            ((:ref ("a") :default "{b") "}"))
     ("${a|${b}}"           ((:ref ("a") :default (:ref ("b")))))
     ("${${a}}"             ((:ref ((:ref ("a"))))))

     ("@{a"                 error)
     ("@{a}"                ((:ref/list ("a"))))
     ("@{a|}"               ((:ref/list ("a") :default nil)))
     ("@{a|b}"              ((:ref/list ("a") :default "b")))
     ("@{a|{b}}"            ((:ref/list ("a") :default "{b") "}"))
     ("@{a|${b}}"           ((:ref/list ("a") :default (:ref ("b")))))
     ("@{a|a${b}}"          ((:ref/list ("a") :default ("a" (:ref ("b"))))))
     ("@{@{a}}"             ((:ref/list ((:ref/list ("a"))))))

     ("$(a"                 error)
     ("$()"                 error)
     ("$(a)"                ((:call "a")))
     ("$(a b)"              ((:call "a" "b")))
     ("$(a \"b c\")"        ((:call "a" "b c")))
     ("$(a [b, c])"         ((:call "a" (:list "b" "c"))))
     ("$(a [\"b${c}\", d])" ((:call "a" (:list ("b" (:ref ("c"))) "d"))))
     ("$(a $(b))"           ((:call "a" (:call "b"))))

     ("@(a"                 error)
     ("@()"                 error)
     ("@(a)"                ((:call/list "a")))
     ("@(a b)"              ((:call/list "a" "b")))
     ("@(a \"b c\")"        ((:call/list "a" "b c")))
     ("@(a [b, c])"         ((:call/list "a" (:list "b" "c"))))
     ("@(a [\"b@{c}\", d])" ((:call/list "a" (:list ("b" (:ref/list ("c"))) "d"))))
     ("@(a @(b))"           ((:call/list "a" (:call/list "b"))))

     ("foo${a}"             ("foo" (:ref ("a")))))))
