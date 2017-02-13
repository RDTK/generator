(cl:in-package #:jenkins.language-server)

(defmethod process-method ((object context)
                           (method (eql :initialize))
                           &key
                           process-id
                           root-uri
                           root-path
                           capabilities)
  '((:capabilities . ((:text-document-sync  . ((:change             . 2)))
                      (:completion-provider . ((:resolve-provider   . t)
                                               (:trigger-characters . (":"))))))))

(defmethod process-method ((object context)
                           (method (eql :initialized))
                           &key))

(defmethod process-method ((object context)
                           (method (eql :shutdown))
                           &key))

(defmethod process-method ((object context)
                           (method (eql :exit))
                           &key)
  :exit)

(defmethod process-method ((object context)
                           (method symbol)
                           &rest args &key)
  (when-let* ((name      (symbol-name method))
              (index     (position #\/ name))
              (interface (make-keyword (subseq name 0 index))) ; TODO use lookup
              (method    (make-keyword (subseq name (1+ index)))))
    (apply #'process-interface-method object interface method args)))
