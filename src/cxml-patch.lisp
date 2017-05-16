(cl:in-package #:xpath)

(defun parse-xpath (str)
  "@arg[str]{a string}
   @return{a s-expression-based XPath expression}
   Parses a string-based XPath expression into s-expression-based one."
  (let ((str (if (typep str 'base-string)
                 (make-array (length str) :element-type 'character :initial-contents str)
                 str)))
    (handler-bind
        ((error
          (lambda (c)
            (unless (typep c 'xpath-error)
              (xpath-error "invalid XPath syntax: ~A in: ~A" c str)))))
      (yacc:parse-with-lexer (make-fixup-lexer (xpath-lexer str))
                             *xpath-parser*))))

(cl:in-package #:cxml-dom)

(defun adjust-vector-exponentially (vector new-dimension set-fill-pointer-p)
  (let ((d (array-dimension vector 0)))
    (when (< d new-dimension)
      (loop do (setf d (max 1 (* 2 d)))
         while (< d new-dimension))
      (adjust-array vector d))
    (when set-fill-pointer-p
      (setf (fill-pointer vector) new-dimension))))
