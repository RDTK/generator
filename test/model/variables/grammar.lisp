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

   '((""           nil)
     ("foo"        ("foo"))
     ("foo$bar"    ("foo$bar"))
     ("foo{bar"    ("foo{bar"))
     ("foo}bar"    ("foo}bar"))
     ("foo\\${"    ("foo${"))
     ("foo\\@{"    ("foo@{"))

     ("${a"        error)
     ("${a}"       ((:ref ("a"))))
     ("${a|}"      ((:ref ("a") :default nil)))
     ("${a|b}"     ((:ref ("a") :default "b")))
     ("${a|{b}}"   ((:ref ("a") :default "{b") "}"))
     ("${a|${b}}"  ((:ref ("a") :default (:ref ("b")))))
     ("${${a}}"    ((:ref ((:ref ("a"))))))

     ("@{a"        error)
     ("@{a}"       ((:ref/list ("a"))))
     ("@{a|}"      ((:ref/list ("a") :default nil)))
     ("@{a|b}"     ((:ref/list ("a") :default "b")))
     ("@{a|{b}}"   ((:ref/list ("a") :default "{b") "}"))
     ("@{a|${b}}"  ((:ref/list ("a") :default (:ref ("b")))))
     ("@{a|a${b}}" ((:ref/list ("a") :default ("a" (:ref ("b"))))))
     ("@{@{a}}"    ((:ref/list ((:ref/list ("a"))))))

     ("foo${a}"    ("foo" (:ref ("a"))))

     ("\\${find \\{ \\}}"   ("${find \\{ \\}}"))

     ;; Function call syntax
     ("$(foo bar baz ${foo} $(whoop ${a|$(call)}))" ((:call ("foo") ("bar") ("baz") (:ref ("foo")))))

     ;; Escapes
     ("\\${bar}"            ("${bar}"))
     ("\\$(bar)"            ("$(bar)"))
     ("\\$(find \\( \\\\))" ("$(find \\( \\\\))"))
     ("\\$(foo)"            ("$(foo)"))
     ("\\( \\)"             ("\\( \\)"))
     ("$(foo \\) bar)"      ((:call "foo" ")" "bar"))))))

;; (value-parse "${foo|}")
;;
;; (defclass test (direct-variables-mixin
;;                 jenkins.model:parented-mixin)
;;   ())
;;
;;
;;
;;
;; (let* ((pa (make-instance 'test
;;                           :variables `((:y . ,(value-parse "b"))
;;                                        (:z . ,(value-parse '(-5))))))
;;
;;        (ci (make-instance 'test
;;                           :parent pa
;;                           :variables `((:a   . 1)
;;                                        (:b   . ,(value-parse '(2 3)))
;;                                        (:foo . ,(lambda (&rest args)
;;                                                   (value-parse args)))
;;                                        (:c   . ,(value-parse "$(foo ${a} @{b})"))
;;
;;                                        (:x   . ,(value-parse (json:decode-json-from-string
;;                                                               "{ \"a\": { \"b\": \"$(x c)\" }, \"c\": [ 4, 5 ] }")))
;;                                        (:y   . ,(value-parse '("a" "${next-value}")))
;;                                        (:z   . ,(value-parse (json:decode-json-from-string
;;                                                                "[ 1, 2, \"@(x @{y})\", \"@{next-value}\" ]")))))))
;;   (values (value ci :c) (value ci :z)))
;;
;; (push :z *traced-variables*)
