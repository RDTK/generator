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

;; `wrap-shell-command' smoke test

(flet ((test-case (expected command pre post)
         (let ((actual (with-output-to-string (stream)
                         (wrap-shell-command stream "foo" nil   nil))))
           (assert (string= expected actual)))))
  (test-case "foo"       "foo" nil   nil)
  (test-case "foobaz"    "foo" nil   "baz")
  (test-case "barfoo"    "foo" "bar" nil)
  (test-case "barfoobaz" "foo" "bar" "baz")

  (test-case (format nil "#!/bin/sh~%foo")
             (format nil "#!/bin/sh~%foo") nil   nil)
  (test-case (format nil "#!/bin/sh~%foobaz")
             (format nil "#!/bin/sh~%foo") nil   "baz")
  (test-case (format nil "#!/bin/sh~%barfoo")
             (format nil "#!/bin/sh~%foo") "bar" nil)
  (test-case (format nil "#!/bin/sh~%barfoobaz")
             (format nil "#!/bin/sh~%foo") "bar" "baz"))

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
