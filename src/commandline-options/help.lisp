;;;; help.lisp --- Help text generation for the commandline-options module.
;;;;
;;;; Copyright (C) 2017, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.project.commandline-options)

(defun print-option (stream info colon? at?)
  (option-synopsis info stream :long? colon?)
  (when at?
    (format stream "~%~
                    ~2@T~:[~
                      <not documented>~
                    ~;~
                      ~:*~<~/jenkins.project.commandline-options::print-documentation/~:>~
                    ~]"
            (when-let* ((option        (option info))
                        (documentation (documentation option t)))
              (list
               (case (option-multiplicity info)
                 (* (format nil "~A~2%This option can be supplied multiple times."
                            documentation))
                 (t documentation)))))))

(defun print-options (stream options colon? at?)
  (declare (ignore colon? at?))
  (format stream "~{~@:/jenkins.project.commandline-options::print-option/~^~2%~}"
          (sort (copy-list options)
                (rcurry #'designators< :positional-first? t)
                :key (compose #'designators))))

(defun print-usage (stream options colon? at?)
  (declare (ignore colon? at?))
  (let* ((interesting (remove-if-not (disjoin (of-type 'positional-option-info)
                                              #'mandatory?)
                                     options))
         (interesting (sort (copy-list interesting) #'designators<
                            :key #'designators))
         (boring      (set-difference options interesting :test #'eq)))
    (format stream "~:[~; [COMMAND-OPTIONS]~]~
                    ~{ ~/jenkins.project.commandline-options::print-option/~}"
            boring interesting)))

;;; Utilities

(defun designator< (left right &key positional-first?)
  (etypecase left
    ((and positional-option-designator (not (eql &rest)))
     (typecase right
       ((and positional-option-designator (not (eql &rest)))
        (< left right))
       (t
        positional-first?)))
    ((eql &rest)
     (typecase right
       (named-option-designator positional-first?)))
    (named-option-designator
     (typecase right
       (named-option-designator (string< left right))
       (t                       (not positional-first?))))))

(defun designators< (left right &key positional-first?)
  (designator< (first left) (first right) :positional-first? positional-first?))

(defun split-into-paragraphs (string)
  (labels ((make-paragraph (characters)
             (if (eql (first characters) #\Newline)
                 (make-paragraph (rest characters))
                 (coerce (nreverse characters) 'string))))
    (loop :with paragraph =       '()
          :for  previous  =       nil :then current
          :for  current   :across string
          :if (and (eql previous #\Newline) (eql current #\Newline))
            :collect (make-paragraph paragraph) :into paragraphs
            :and :do (setf paragraph '())
          :else
            :do (push current paragraph)
          :finally (return (append paragraphs
                                   (list (make-paragraph paragraph)))))))

(assert (equal (split-into-paragraphs "foo bar
baz fez

whoop di
do")
               '("foo bar
baz fez"
                 "whoop di
do")))

(defun print-documentation (stream documentation &optional colon? at?)
  "`pprint-fill' the words in DOCUMENTATION onto STREAM."
  (declare (ignore colon? at?))
  (loop :with *print-escape* = nil
        :with first? = t
        :for paragraph :in (split-into-paragraphs documentation)
        :unless first? :do
           (pprint-newline :mandatory stream)
           (pprint-newline :mandatory stream)
        :do
           (setf first? nil)
           (loop :with lines  =   (split-sequence:split-sequence #\Newline paragraph)
                 :with first? =   t
                 :for  line   :in lines
                 :for  words  =   (split-sequence:split-sequence #\Space line)
                 :do (if first?
                         (setf first? nil)
                         (pprint-newline :mandatory stream))
                     (pprint-fill stream words nil))))
