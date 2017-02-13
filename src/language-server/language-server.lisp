;;;; https://github.com/Microsoft/language-server-protocol/

(cl:in-package #:jenkins.language-server)

(defun language-server (input output)
  (catch 'exit
    (with-output-to-file (*trace-output* "/tmp/trace" :if-exists :supersede)
      (loop :with context = (make-instance 'context)
         :do (process-request input output context)))))

(defun process-request (input output context)
  (let+ ((request/raw (read-request input))
         (request     (json:decode-json-from-string request/raw))

         (id          (assoc-value request :id))
         (method      (make-keyword (string-upcase (assoc-value request :method))))
         (arguments   (alist-plist (assoc-value request :params)))

         ((&values result condition)
          (ignore-errors (apply #'process-method context method arguments)))
         (response
          (when id
            (json:encode-json-to-string
             `((:jsonrpc . "2.0")
               (:id      . ,id)
               ,(if condition
                    `(:error  . ((:code    . 0)
                                 (:message . ,(princ-to-string condition))))
                    `(:result . ,result)))))))

    (when condition
      (format *trace-output* "Error: ~A~%" condition)
      (force-output *trace-output*))

    (print request *trace-output*)
    (print response *trace-output*)
    (force-output *trace-output*)

    (case response
      ((nil))
      (:exit
       (throw 'exit nil))
      (t
       (write-response output response)))))




;;; TOOO move somewhere

(defmethod complete ((context (eql :variable-name))
                     (prefix  string))
  (let+ (((&flet make-item (variable)
            (let ((name          (jenkins.model.variables:variable-info-name variable))
                  (type          (jenkins.model.variables:variable-info-type variable))
                  (documentation (jenkins.model.variables:variable-info-documentation variable)))
              `((:insert-text   . ,(typecase type
                                     ((eql string)
                                      (format nil "~(~A~): \"\""
                                              name))
                                     ((cons (eql jenkins.model::list-of) (cons (eql string) null))
                                      (format nil "~(~A~): [ \"\" ]"
                                              name))
                                     (t
                                      (format nil "~(~A~): " name))))
                (:label         . ,(string-downcase name))
                (:detail        . ,(format nil "Type: ~A " type))
                (:documentation . ,documentation))))))
    `((:is-complete . t)
      (:items       . ,(loop :for variable :in (jenkins.model.variables:all-variables)
                          :when (starts-with-subseq
                                 prefix (string-downcase (jenkins.model.variables:variable-info-name variable)))
                          :collect (make-item variable))))))

(defmethod complete ((context (eql :variable-value))
                     (prefix  string))
  (let+ ((variable (nth 3 (jenkins.model.variables:all-variables)))
         ((&flet make-item (label)
            `((:label . ,label)))))
    `((:is-complete . t)
      (:items       . ,(typecase (jenkins.model.variables:variable-info-type variable)
                         ((eql boolean)
                          (list (make-item "true") (make-item "false")))
                         ((eql string)
                          (list (make-item "\"\"")))
                         ((cons (eql jenkins.model::list-of) (cons (eql string) null))
                          (list (make-item "[ \"\" ]"))))))))
