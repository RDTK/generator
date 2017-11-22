#.(progn
    #1=(ql:quickload :project-automation.frontend)
    '#1#)

;;;; context-methods.lisp --- TODO.
;;;;
;;;; Copyright (C) 2016, 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.language-server)

(defmethod process-method ((object context)
                           (method (eql :initialize))
                           &key
                           process-id
                           root-uri
                           root-path
                           capabilities)
  (setf (%workspace object) (make-instance 'workspace
                                           :root-uri  root-uri
                                           :root-path root-path))
  '((:capabilities . ((:text-document-sync          . 2)
                      (:hover-provider              . t)
                      (:completion-provider         . ((:resolve-provider   . t)
                                                       (:trigger-characters . (":"))))
                      (:document-highlight-provider . t)))))

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

;;; Dispatching to workspace, etc.

(defmethod process-method ((object context) (method string)
                           &rest args &key)
  (let+ (((&flet as-keyword (string)
            (find-symbol (string-upcase string) (find-package '#:keyword))))
         (index            (position #\/ method))
         (interface/symbol (when index
                             (as-keyword (subseq method 0 index)))) ; TODO use lookup
         (method/symbol    (as-keyword (if index
                                           (subseq method (1+ index))
                                           method))))
    (cond
      ((eq interface/symbol :$) ; TODO
       )
      ((and interface/symbol method/symbol)
       (apply #'process-interface-method
              object interface/symbol method/symbol args))
      (method/symbol
       (apply #'process-method object method/symbol args))
      (t
       (error "No such method: \"~A\"." method)))))

(defmethod process-interface-method ((object    context)
                                     (interface t)
                                     (method    t)
                                     &rest args &key)
  (apply #'process-interface-method (workspace object) interface method args))

;;; Hack

;; TODO move into workspace method?
(defmethod process-interface-method :after ((object     context)
                                            (infterface (eql :textdocument))
                                            (method     (eql :didchange))
                                            &key
                                              text-document)
  (let+ (((&values uri version) (parse-text-document text-document))
         (document (find-document uri (workspace object)))
         (diagnostics '())
         ((&flet position* (location which)
            `((:line      . ,(or (rs.f:line location :of which) 1))
              (:character . ,(or (rs.f:column location :of which) 1)))))
         ((&flet range (location)
            `((:start . ,(position* location :start))
              (:end   . ,(position* location :start)))))
         ((&flet diagnostic (location message severity)
            `((:range    . ,(range location))
              (:severity . ,severity)
              (:message  . ,(princ-to-string message)))))
         ((&flet notify ()
            (write-notification
             (connection object) "textDocument/publishDiagnostics"
             `((:uri         . ,uri)
               (:diagnostics . ,(or diagnostics #() ; HACK
                                    )))))))
    (princ document *trace-output*)
    (princ (text document) *trace-output*)
    (with-simple-restart (abort "~@<Abort processing~@:>")
      (handler-bind
          ((rosetta.frontend:parse-warning
            (lambda (condition)
              (let+ ((location (rosetta.frontend:location condition))
                     (message  (more-conditions:cause condition)))
                (describe location *trace-output*)
                (format *trace-output* "~A~%" location)
                (push (diagnostic location message 2) diagnostics))
              (muffle-warning condition)))
           (rosetta.frontend:parse-error1
            (lambda (condition)
              (let+ ((location (rosetta.frontend:location condition))
                     (message  (more-conditions:cause condition)))
                (describe location *trace-output*)
                (format *trace-output* "~A~%" location)
                (push (diagnostic location message 1) diagnostics))
              (continue condition)
              (abort))))
        (model.transform.trace:with-tracer ((model.transform.trace:make-tracer))
          (let ((resolver  (make-instance 'rs.f:search-path-resolver
                                           :search-path `(,#P"~/code/citec/citk/recipes-next/templates/toolkit/"
                                                          ,#P"~/code/citec/citk/recipes-next/projects/"
                                                          ,#P"~/code/citec/citk/recipes-next/distributions/"))
                            #+no (make-instance 'language-server-resolver))

                (repository (make-instance 'rs.m.d::base-repository)))
            (rs.f:process (language document)
                          (make-string-input-stream (text document))
                          `(:model :resolver   ,resolver
                                   :repository ,repository)
                          :location-name uri)))))

    (notify)))
