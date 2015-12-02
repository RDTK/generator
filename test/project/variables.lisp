(progn
  (parse "a@{b}c")
  (expand (parse "a${VAR:BVAR:B}c")
          (lambda (x)
            (optima:ematch x
              (:b           "var:b")
              (:|VAR:BVAR:B| (list "d" "e")))))

  (expand (parse "a@{${b}${b}}c")
          (lambda (x)
            (optima:ematch x
              (:b           "var:b")
              (:|VAR:BVAR:B| (list "d" "e")))))

  (expand (parse (list "a" "${${b}${b}}" "c"))
          (lambda (x)
            (optima:ematch x
              (:b            "var:b")
              (:|VAR:BVAR:B| (list (list "d" "e") (list "e" "f"))))))

  (expand (parse (list "a" "@{${b}${b}}" "c"))
          (lambda (x)
            (optima:ematch x
              (:b            "var:b")
              (:|VAR:BVAR:B| (list (list "d" "e") (list "e" "f"))))))

  (expand (parse (list "a" "${${b}${b}}" "c"))
          (lambda (x)
            (optima:ematch x
              (:b            "var:b")
              (:|VAR:BVAR:B| (list "d" "e"))))))

#+no (let ((a (make-instance 'version-spec
                             :name "bla"
                             :variables '(:a1 ("foo" "${b2} ${b2}" "bar")
                                          :a2 "<@{b2}>"
                                          :a3 "${a2}"
                                          :b1 "${c}"
                                          :b2 "@{c}"
                                          :b3 ("foo" "@{c}" "bar")
                                          :c ("a" "b")))))
       (values-list (mapcar (lambda (x)
                              (expand (parse (format nil "${~A}" x))
                                      (lambda (y) (lookup a y))))
                            '(:a1 :a2 :a3 :b1 :b2 :b3 :c))))

#+test (let ((a (make-instance 'version-spec
                               :name "bla"
                               :variables '(:a1 ("foo" "${b2} ${b2}" "bar")
                                            :a2 "<@{b2}>"
                                            :a3 "${a2}"
                                            :b1 "${c}"
                                            :b2 "@{c}"
                                            :b3 ("foo" "@{c}" "bar")
                                            :c ("a" "b")))))
         (values-list (mapcar (curry #'value a) '(:a1 :a2 :a3 :b1 :b2 :b3 :c))))
;; exptected:
;; ("foo" "ab ab" "bar")
;; (("<" "a" ">") ("<" "b" ">"))
;; "<a><b>"
;; "ab"
;; ("a" "b")
;; ("foo" ("a" "b") "bar")
;; ("a" "b")

;;; Newer Tests

(defclass foo (direct-variables-mixin
               parented-mixin)
  ())

(let* ((distribution (make-instance
                      'foo
                      :variables ()))
       (project      (make-instance
                      'foo
                      :parent    distribution
                      :variables (list :foo "${next-value|ABC}"
                                       :shell.command "echo ${foo}
echo ${bar|XYZ}
"))))
  (value project :shell.command))
