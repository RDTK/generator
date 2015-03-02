(cl:in-package #:jenkins.model.aspects.test)

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
               (let ((*builder-constraints* (make-hash-table)))
                 (setf (gethash :a *builder-constraints*) spec-a
                       (gethash :b *builder-constraints*) spec-b)
                 (if no-relation?
                     (assert (not (builder< :a :b *builder-constraints*)))
                     (assert (builder< :a :b *builder-constraints*)))
                 (assert (not (builder< :b :a *builder-constraints*)))))))

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
