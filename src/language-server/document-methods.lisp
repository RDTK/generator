;;;; document-methods.lisp --- TODO.
;;;;
;;;; Copyright (C) 2016, 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.language-server)

;;;

(defmethod process-interface-method ((context   context) ; TODO move to context-methods.lisp?
                                     (interface (eql :textdocument))
                                     (method    (eql :didopen))
                                     &key
                                     text-document)
  (let+ (((&values uri version) (parse-text-document text-document))
         ;; (language-id :language-id)
         (text (assoc-value text-document :text)))
    (log:info "new document" uri version text)
    (setf (find-document uri context) (make-instance 'document
                                                     :version version
                                                     :text    text))
    (log:info "registered" #+no (newlines (find-document uri context)))))

(defmethod process-interface-method ((context   context)
                                     (interface (eql :textdocument))
                                     (method    (eql :didclose))
                                     &key
                                     text-document)
  (let ((uri (parse-text-document text-document)))
    (setf (find-document uri context) nil)))

(defmethod process-interface-method ((context   context)
                                     (interface (eql :textdocument))
                                     (method    t)
                                     &rest args &key text-document)
  (let+ (((&values uri version) (parse-text-document text-document))
         (document (find-document uri context)))
    (apply #'process-method document method
           :version version (remove-from-plist args :text-document))))

;;;

(defmethod process-method ((object document)
                           (method (eql :didsave))
                           &key
                           ))

(defmethod process-method ((object document)
                           (method (eql :didchange))
                           &key
                           content-changes)
  (let+ (((&flet apply-change (change)
            (let+ (((text (&optional start-line start-column end-line end-column) length)
                    (parse-text-document-content-change change)))

              (log:info "~D:~D - ~D:~D -> ~S~%"
                        start-line start-column end-line end-column text)

              (when-let ((thread (find "repl-thread" (bt:all-threads)
                                       :key #'bt:thread-name :test #'string=)))
                (bt:interrupt-thread
                 thread
                 (lambda ()
                   (swank:eval-in-emacs
                    `(with-current-buffer (get-buffer-create "*lsp-buildgen-debug*")
                       (erase-buffer)
                       (insert ,(text object)))
                    t))))

              (update object
                      (position->index object start-line start-column)
                      (position->index object end-line end-column)
                      text)))))
    (map nil #'apply-change content-changes)))

(defmethod process-method ((object document)
                           (method (eql :hover))
                           &key
                           position)
  (let+ (((&values line column) (parse-position position))
         (word (word-at object (cons line column)))) ; TODO return range
    (log:info word line column)
    (if-let ((info (jenkins.model.variables:find-variable (make-keyword (string-upcase word))))) ; TODO must sent hover reply
      (let* ((name          (jenkins.model.variables:variable-info-name info))
             (type          (jenkins.model.variables:variable-info-type info))
             (documentation (jenkins.model.variables:variable-info-documentation info))
             (description   (format nil "~(~A~): ~(~A~)~2%~A" name type documentation))) ; TODO the commandline-options system has a function for breaking a documentation string into paragraphs and rendering them
        `((:contents . ,description)
          #+optinal (:range    . ))))))

(defmethod process-method ((object document)
                           (method (eql :completion))
                           &key
                           position)
  (let* ((text (word-at object (multiple-value-call #'cons (parse-position position)))))
    (format *trace-output* "~S" text)
    (complete :variable-name text)
    #+no (complete :variable-value)))

(defmethod process-method ((object document)
                           (method (eql :definition))
                           &key
                           position)
  )

(defmethod process-method ((object document)
                           (method (eql :symbol-highlight))
                           &key)
  )
