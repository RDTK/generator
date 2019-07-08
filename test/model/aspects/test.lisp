(cl:in-package #:build-generator.model.aspects.test)

;; `make-remove-directory-contents/unix' smoke test

(assert
 (string= (make-remove-directory-contents/unix)
          "find . -mindepth 1 -maxdepth 1 -exec rm -rf {} \\;"))
(assert
 (string= (make-remove-directory-contents/unix :exclude "foo")
          "find . -mindepth 1 -maxdepth 1 -not -name \"foo\" -exec rm -rf {} \\;"))
(assert
 (string= (make-remove-directory-contents/unix :exclude '("b\"ar" "foo"))
          "find . -mindepth 1 -maxdepth 1 -not \\( -name \"b\\\"ar\" -o -name \"foo\" \\) -exec rm -rf {} \\;"))

;; `wrapped-shell-command' smoke test

(assert (string= (wrap-shell-command "foo" nil   nil)   "foo"))
(assert (string= (wrap-shell-command "foo" nil   "baz") "foobaz"))
(assert (string= (wrap-shell-command "foo" "bar" nil)   "barfoo"))
(assert (string= (wrap-shell-command "foo" "bar" "baz") "barfoobaz"))

(assert (string= (wrap-shell-command (format nil "#!/bin/sh~%foo") nil   nil)
                 (format nil "#!/bin/sh~%foo")))
(assert (string= (wrap-shell-command (format nil "#!/bin/sh~%foo") nil   "baz")
                 (format nil "#!/bin/sh~%foobaz")))
(assert (string= (wrap-shell-command (format nil "#!/bin/sh~%foo") "bar" nil)
                 (format nil "#!/bin/sh~%barfoo")))
(assert (string= (wrap-shell-command (format nil "#!/bin/sh~%foo") "bar" "baz")
                 (format nil "#!/bin/sh~%barfoobaz")))

;; `parse-constraint' smoke test

(mapc (lambda+ ((json expected))
        (assert (equal expected (parse-constraint
                                 (json:decode-json-from-string json)))))
      '(("[ \"before\", \"<all>\"               ]"                  (:before t t))
        ("[ \"before\", \"foo\"                 ]"                  (:before foo t))
        ("[ \"before\", { \"type\": \"foo\" }   ]"                  (:before foo t))
        ("[ \"before\", { \"type\": \"<all>\" } ]"                  (:before t t))
        ("[ \"before\", { \"name\": \"bar\" }   ]"                  (:before t "bar"))
        ("[ \"before\", { \"name\": \"<all>\" } ]"                  (:before t t))
        ("[ \"before\", { \"type\": \"fez\", \"name\": \"baz\" } ]" (:before fez "baz"))))

;; `builder<' smoke test

(let+ (((&flet check-case (spec-a spec-b &optional no-relation?)
          (log:info spec-a spec-b)
          (let* ((*step-constraints* '())
                 (constraints (constraints-table 'build)))
            (setf (gethash :a constraints) spec-a
                  (gethash :b constraints) spec-b)
            (if no-relation?
                (assert (not (step< :a :b constraints)))
                (assert (step< :a :b constraints)))
            (assert (not (step< :b :a constraints)))))))

      (check-case '(aspect-a "name-a" ((:before t)))
                  '(aspect-b "name-b" ()))
      (check-case '(aspect-a "name-a" ((:before t)))
                  '(aspect-b "name-b" ((:before t)))
                  t)
      (check-case '(aspect-a "name-a" ((:before aspect-b)))
                  '(aspect-b "name-b" ()))
      (check-case '(aspect-a "name-a" ((:before aspect-b)))
                  '(aspect-b "name-b" ((:before t))))
      (check-case '(aspect-a "name-a" ((:before aspect-b "name-b")))
                  '(aspect-b "name-b" ((:before aspect-a)))))
