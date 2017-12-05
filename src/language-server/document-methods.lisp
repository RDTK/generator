;;;; document-methods.lisp --- TODO.
;;;;
;;;; Copyright (C) 2016, 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.language-server)

;;;

(defmethod process-method ((object document)
                           (method (eql :willsave))
                           &key
                             ))

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

              (update object
                      (when (and start-line start-column)
                        (position->index object start-line start-column))
                      (when (and end-line end-column)
                        (position->index object end-line end-column))
                      text)

              #+no (when-let ((thread (find "repl-thread" (bt:all-threads)
                                       :key #'bt:thread-name :test #'string=)))
                (bt:interrupt-thread
                 thread
                 (lambda ()
                   (swank:eval-in-emacs
                    `(with-current-buffer (get-buffer-create "*lsp-buildgen-debug*")
                       (erase-buffer)
                       (insert ,(text object)))
                    t))))

              ))))
    (map nil #'apply-change content-changes)))

;;; Deleting first line
(let ((d (make-instance 'document
                        :language :foo
                        :version 1
                        :text "variables:
foo")))
  (process-method d :didchange :content-changes '(((:RANGE (:START (:LINE . 0) (:CHARACTER . 0))
                                                           (:END (:LINE . 0) (:CHARACTER . 10)))
                                                   (:RANGE-LENGTH . 10) (:TEXT . ""))))
  (process-method d :didchange :content-changes '(((:RANGE (:START (:LINE . 1) (:CHARACTER . 0))
                                                    (:END  (:LINE . 1) (:CHARACTER . 0)))
                                                   (:RANGE-LENGTH . 0)
                                                   (:TEXT . "a"))))
  (text d))

(defmethod process-method ((object document)
                           (method (eql :hover))
                           &key
                           position)
  (let ((position (multiple-value-call #'cons (parse-position position))))
    (compute-scopes object position)
    (make-hash-table))

  #+old (let+ (((&values line column) (parse-position position))
         (word (word-at object (cons line column)))) ; TODO return range
    (if-let ((info (jenkins.model.variables:find-variable (make-keyword (string-upcase word))))) ; TODO must sent hover reply
      (let* ((name          (jenkins.model.variables:variable-info-name info))
             (type          (jenkins.model.variables:variable-info-type info))
             (documentation (jenkins.model.variables:variable-info-documentation info))
             (description   (format nil "~(~A~): ~(~A~)~2%~A" name type documentation))) ; TODO the commandline-options system has a function for breaking a documentation string into paragraphs and rendering them
        `((:contents . ,description)
          #+optinal (:range    . )))
      (make-hash-table)                 ; HACK
      )))

(defmethod process-method ((object document)
                           (method (eql :completion))
                           &key
                           position)
  (let ((word (word-at object (multiple-value-call #'cons (parse-position position)))))
    (complete :variable-name word)
    #+no (complete :variable-value)))

(defmethod process-method ((object document)
                           (method (eql :definition))
                           &key
                           position)
  (let+ (((&values line column) (parse-position position))
         (word (word-at object (cons line column))))
    ))

(defmethod process-method ((object document)
                           (method (eql :symbol-highlight)) ; TODO does this exist?
                           &key)
  )

(defmethod process-method ((object document)
                           (method (eql :documenthighlight))
                           &key
                           version
                           position)
  (let+ (((&values line column) (parse-position position))
         (word (word-at object (cons line column)))
         ((&flet index-position (index)
            (let+ (((&values line column) (index->position object index)))
              `((:line      . ,line)
                (:character . ,column))))))
    (if word
        (loop :with text = (text object)
           :for previous = 0 :then (+ index (length word))
           :for index = (search word text :start2 previous)
           :while index
           :collect `((:range . ((:start . ,(index-position index))
                                 (:end   . ,(index-position (+ index (length word))))))
                      (:kind  . ,(1+ (random 3)))))
        (vector))))
