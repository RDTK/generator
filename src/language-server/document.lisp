;;;; document.lisp --- TODO.
;;;;
;;;; Copyright (C) 2016, 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.language-server)

(defclass document ()
  ((version  :initarg  :version
             :type     non-negative-integer
             :reader   version)
   (text     :type     string
             :reader   text
             :writer   (setf %text)
             :documentation
             "Stores the current text of the document.")
   (newlines :type     vector
             :reader   newlines
             :initform (make-array 100 :fill-pointer 0 :adjustable t)
             :documentation
             "Stores positions of newlines as indices into the string
              stored in the `text' slot.")))

(defmethod shared-initialize :after ((instance   document)
                                     (slot-names t)
                                     &key
                                     (text nil text-supplied?))

  (when text-supplied?
    (setf (%text instance) text)))

(defmethod (setf %text) :after ((new-value string)
                                (document  document))
  (let+ (((&accessors-r/o text newlines) document))
    (setf (fill-pointer newlines) 0)
    (unless (emptyp text)
      (loop :for previous = 0 :then next
         :for next = (position #\Newline text :start (1+ previous))
         :while next
         :do (vector-push-extend next newlines)))))

(defmethod update ((document    document)
                   (start-index integer)
                   (end-index   integer)
                   (new-text    string))
  (log:info "updated" start-index end-index new-text)
  (let ((text (text document)))
    (setf (%text document)
          (concatenate 'string
                       (subseq text 0 start-index)
                       new-text
                       (subseq text end-index)))))

(defmethod position->index ((document  document)
                            (line      integer)
                            (character integer))
  (+ (if (plusp line)
         (1+ (aref (newlines document) (1- line)))
         0)
     character))

;;; Utilities

(defmethod word-at ((document document)
                    (position integer))
  (let+ (((&flet not-word? (character)
            (not (or (member character '(#\. #\? #\-) :test #'char=)
                     (alphanumericp character)))))
         (text  (text document))
         (start (1+ (or (position-if #'not-word? text
                                     :end position :from-end t)
                        -1)))
         (end   (or (position-if #'not-word? text :start position)
                    (length text))))
    (subseq text start end)))

(defmethod word-at ((document document)
                    (position cons))
  (let+ (((line . character) position))
    (word-at document (position->index document line character))))
