;;;; conversion.lisp --- Conversions used by the api module.
;;;;
;;;; Copyright (C) 2012-2022 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.api)

;;; Boolean

(define-enum-type (boolean :test string-equal :deftype? nil)
  (nil "false" "no")
  (t   "true"  "yes"))

;;; Boolean encoded by presence of an Element

(deftype boolean/element (local-name)
  (declare (ignore local-name))
  '(member t nil))

(defun local-name-and-child (element type-spec)
  (let+ (((local-name) type-spec)
         ((&flet local-name-matches? (child)
            (and (typep child 'stp:element)
                 (string= (stp:local-name child) local-name)))))
    (values local-name (stp:find-child-if #'local-name-matches? element))))

(defmethod xloc:xml-> ((value stp:element)
                       (type  (eql 'boolean/element))
                       &key inner-types &allow-other-keys)
  (let+ (((&values &ign child) (local-name-and-child value inner-types)))
    (when child t)))

(defmethod xloc:->xml ((value t)
                       (dest  stp:element)
                       (type  (eql 'boolean/element))
                       &key inner-types &allow-other-keys)
  (let+ (((&values local-name child)
          (local-name-and-child dest inner-types)))
    (cond
      ((not value)
       (when child
         (stp:delete-child child dest)))
      ((not child)
       (stp:append-child dest (stp:make-element local-name)))))
  value)

;;; Keyword

(defmethod xloc:xml-> ((value string)
                       (type  (eql 'keyword))
                       &key &allow-other-keys)
  (make-keyword value))

(defmethod xloc:->xml ((value symbol)
                       (dest  (eql 'string))
                       (type  (eql 'keyword))
                       &key &allow-other-keys)
  (symbol-name value))

;;; Keyword with downcasing

(deftype keyword/downcase (&rest members)
  (or 'keyword `(member ,@members)))

(defmethod xloc:xml-> ((value string)
                       (type  (eql 'keyword/downcase))
                       &key &allow-other-keys)
  (make-keyword (string-upcase value)))

(defmethod xloc:->xml ((value symbol)
                       (dest  (eql 'string))
                       (type  (eql 'keyword/downcase))
                       &key &allow-other-keys)
  (string-downcase value))

;;; `equals-string/cons'

(deftype equals-string/cons (&optional (key 'keyword) (value 'string))
  "A `cl:cons' which should be stored into certain locations of XML
   documents as a string of the form KEY=VALUE."
  `(cons ,key ,value))

(defmethod xloc:xml-> ((value string)
                       (type  (eql 'equals-string/cons))
                       &key
                       inner-types
                       &allow-other-keys)
  (let+ (((&optional (keyword-type 'keyword) (value-type 'string))
          inner-types)
         ((key value) (split-sequence #\= value)))
    (cons (xloc:xml-> key keyword-type) (xloc:xml-> value value-type))))

(defmethod xloc:->xml ((value cons)
                       (dest  (eql 'string))
                       (type  (eql 'equals-string/cons))
                       &key
                       inner-types
                       &allow-other-keys)
  (let+ (((&optional (keyword-type 'keyword) (value-type 'string))
          inner-types))
    (format nil "~A=~A"
            (xloc:->xml (car value) 'string keyword-type)
            (xloc:->xml (cdr value) 'string value-type))))

;;; `string/node'

(deftype string/node ()
  "A `cl:string' which should be stored into certain locations of XML
   documents in a particular way."
  'string)

(defmethod xloc:xml-> ((value stp:element)
                       (type  (eql 'string/node))
                       &key &allow-other-keys)
  (stp:data (stp:first-child value)))

(defmethod xloc:->xml ((value string)
                       (dest  stp:element)
                       (type  (eql 'string/node))
                       &key &allow-other-keys)
  (stp:delete-children dest)
  (stp:append-child dest (stp:text :data value))
  dest)

;;; singleton-element

(deftype singleton-element (path &optional type)
  (declare (ignore path type))
  'string)

(defmethod xloc:xml-> ((value stp:element)
                       (type  (eql 'singleton-element))
                       &key
                       inner-types
                       &allow-other-keys)
  (let+ (((path &optional (type 'string)) inner-types))
    (xloc:with-locations-r/o (((:val value :type type) path)) value
      value)))

(defmethod xloc:->xml ((value t)
                       (dest  stp:element)
                       (type  (eql 'singleton-element))
                       &key
                       inner-types
                       &allow-other-keys)
  (let+ (((path &optional (type 'string)) inner-types))
    (xloc:with-locations (((:val value1 :type type) path)) dest
      (setf value1 value))))

;;; list/{comma,newline,space}
;;;
;;; Lists which are represented as strings with certain separator
;;; characters.

(macrolet
    ((define-separator-string-list-type (separator)
       (let ((name                    (format-symbol
                                       *package* "LIST/~@:(~A~)" (char-name separator)))
             (format-string/aesthetic (format nil "~~{~~A~~^~C~~}" separator))
             (format-string/readable  (format nil "~~{~~S~~^~C~~}" separator)))
         `(progn
            (deftype ,name (&optional element-type)
              (if element-type
                  `(or null (cons ,element-type))
                  'list))

            (defmethod xloc:xml-> ((value string)
                                   (type  (eql ',name))
                                   &key
                                   inner-types
                                   &allow-other-keys)
              (let* ((chunks       (split-sequence ,separator value :remove-empty-subseqs t))
                     (chunks/clean (mapcar (curry #'string-trim '(#\Space #\Tab #\Newline))
                                           chunks))
                     (elements     (remove-if #'emptyp chunks/clean)))
               (mapcar (rcurry #'xloc:xml-> inner-types) elements)))

            (defmethod xloc:->xml ((value list)
                                   (dest  (eql 'string))
                                   (type  (eql ',name))
                                   &key
                                   inner-types
                                   &allow-other-keys)
              (with-standard-io-syntax
                (if (equal inner-types '(string))
                    (format nil ,format-string/aesthetic value)
                    (format nil ,format-string/readable  value))))))))

  (define-separator-string-list-type #\,)
  (define-separator-string-list-type #\Newline)
  (define-separator-string-list-type #\Space))

;;; plist/equals

(deftype plist/equals (&optional list-type key-type value-type)
  (declare (ignore list-type))
  `(or null (cons ,key-type (cons ,(or value-type t)))))

(defmethod xloc:xml-> ((value string)
                       (type  (eql 'plist/equals))
                       &key
                       inner-types
                       &allow-other-keys)
  (let ((element-type (list* 'equals-string/cons (rest inner-types))))
    (loop :for line :in (xloc:xml-> value `(,(first inner-types) string))
          :for (key . value) = (xloc:xml-> line element-type)
          :collect key :collect value)))

(defmethod xloc:->xml ((value list)
                       (dest  (eql 'string))
                       (type  (eql 'plist/equals))
                       &key
                       inner-types
                       &allow-other-keys)
  (let ((element-type (list* 'equals-string/cons (rest inner-types))))
    (xloc:->xml (loop :for (key value1) :on value :by #'cddr
                      :collect (xloc:->xml (cons key value1) dest element-type))
                dest `(,(first inner-types) string))))

;;; equals+newline/plist

(deftype equals+newline/plist (&optional key-type value-type)
  `(or null (cons ,key-type (cons ,(or value-type t)))))

(defmethod xloc:xml-> ((value string)
                       (type  (eql 'equals+newline/plist))
                       &key
                       inner-types
                       &allow-other-keys)
  (let ((element-type (list* 'equals-string/cons inner-types)))
    (loop :for line :in (xloc:xml-> value '(list/newline string))
          :for (key . value) = (with-simple-restart (continue "Skip the line")
                                 (handler-bind
                                     ((xloc:xml->-conversion-error #'continue))
                                   (xloc:xml-> line element-type)))
          :when key
          :collect key :and :collect value)))

(defmethod xloc:->xml ((value list)
                       (dest  (eql 'string))
                       (type  (eql 'equals+newline/plist))
                       &key
                       inner-types
                       &allow-other-keys)
  (let ((element-type (list* 'equals-string/cons inner-types)))
    (xloc:->xml (loop :for (key value1) :on value :by #'cddr
                      :collect (xloc:->xml (cons key value1) dest element-type))
                dest '(list/newline string))))

;;; `tree-map/plist'
;;;
;;; Convert Lisp plists to Jenkins' "tree-map". A "tree-map" looks
;;; like this:
;;;
;;;   <SOME-CONTEXT>
;;;     <int>SIZE-AS-INTEGER</int>
;;;     <string>KEY</string>
;;;     <string>VALUE</string>
;;;     ...
;;;   </SOME-CONTEXT>

(deftype tree-map/plist ()
  "A Lisp plist which should into XML documents as a \"tree-map\"."
  '(or null (cons keyword (cons string))))

(defmethod xloc:xml-> ((value stp:element)
                       (type  (eql 'tree-map/plist))
                       &key &allow-other-keys)
  (xloc:with-locations-r/o
      (((:val items :type 'string/node) "./string"
        :if-multiple-matches :all))
      value
    (loop :for (key value) :on items :by #'cddr
          :collect (make-keyword key) :collect value)))

(defmethod xloc:->xml ((value list)
                       (dest  stp:element)
                       (type  (eql 'tree-map/plist))
                       &key &allow-other-keys)
  (let+ (((&values count remainder) (floor (length value) 2)))
    (unless (zerop remainder)
      (error 'type-error
             :datum         value
             :expected-type 'tree-map/plist))

    (xloc:with-locations
        (((:val size  :type 'integer)     "./int/text()" )
         ((:val items :type 'string/node) "./string"
          :if-multiple-matches :all))
        dest
      (setf size  count
            items (mapcar #'string value))))
  dest)

;;; `stupid-threshold'

(deftype stupid-threshold ()
  '(member :success :unstable :failure))

(defmethod xloc:xml-> ((value stp:element)
                       (type  (eql 'stupid-threshold))
                       &key &allow-other-keys)
  (xloc:with-locations-r/o
      (((:val name            :type 'string)  "./name/text()")
       #+later? ((:val ordinal         :type 'string)  "./ordinal/text()")
       #+later? ((:val color           :type 'string)  "./color/text()")
       #+later? ((:val complete-build? :type 'boolean) "./completeBuild/text()"))
      value
    (cond
      ((string= name "SUCCESS")
       :success)
      ((string= name "UNSTABLE")
       :unstable)
      ((string= name "FAILURE")
       :failure)
      (t
       (error "~@<Unknown threshold name: ~S~@:>" name)))))

(defmethod xloc:->xml ((value symbol)
                       (dest  stp:element)
                       (type  (eql 'stupid-threshold))
                       &key &allow-other-keys)
  (check-type value stupid-threshold)
  (xloc:with-locations
      (((:val name            :type 'string)  "./name/text()")
       ((:val ordinal         :type 'string)  "./ordinal/text()")
       ((:val color           :type 'string)  "./color/text()")
       ((:val complete-build? :type 'boolean) "./completeBuild/text()"))
      dest
    (ecase value
      (:success
       (setf name            (string value)
             ordinal         0
             color           "BLUE"
             complete-build? t))
      (:unstable
       (setf name            (string value)
             ordinal         1
             color           "YELLOW"
             complete-build? t))
      (:failure
       (setf name            (string value)
             ordinal         2
             color           "RED"
             complete-build? nil))))
  dest)

;;; `health-scale-factor'

(deftype health-scale-factor ()
  '(or nil positive-real))

(defmethod xloc:xml-> ((value string)
                       (type  (eql 'health-scale-factor))
                       &key &allow-other-keys)
  (let ((value (with-standard-io-syntax
                 (read-from-string value))))
    (unless (zerop value)
      value)))

(defmethod xloc:->xml ((value real)
                       (dest  (eql 'string))
                       (type  (eql 'health-scale-factor))
                       &key &allow-other-keys)
  (with-standard-io-syntax
    (format nil "~6,,,,,,'EG" (or value 0))))

;;; `access-control-rule'

(deftype access-control-rule ()
  "Values are of the form

     (SUBJECT (ACTION1 ACTION2 ...) [:kind {:user | :group}])

   where SUBJECT is the name of the user or group as a string and
   ACTION1, ACTION2, ... are keywords naming the action in question.

   A typical example looks like this:

     (\"joeuser\" (:item :build) :kind :user)

   ."
  `(cons string (cons (cons (or keyword string) list)
                      (or null (cons (eql :kind)
                                     (cons (member :user :group) null))))))

(assert      (typep '("joeuser" ("action1" "action2"))                  'access-control-rule))
(assert (not (typep '("joeuser" ())                                     'access-control-rule)))
(assert      (typep '("joeuser" ("action1" "action2") :kind :user)      'access-control-rule))
(assert (not (typep '("joeuser" ("action1" "action2") :kind :something) 'access-control-rule)))

(declaim (ftype (function (string) (values access-control-rule &optional nil))
                parse-permission))
(defun parse-permission (string)
  (let+ (((&values success? groups)
          (ppcre:scan-to-strings "^(?:([^:]+):)?([^:]+):(.+)$" string)))
    (when success?
      (let+ ((#(kind/raw action/raw subject) groups)
             (kind   (when kind/raw
                       (or (find-symbol kind/raw '#:keyword)
                           (error "Unknown permission kind ~S" kind/raw))))
             (action (if (starts-with-subseq #1="hudson.model." action/raw)
                         (mapcar (compose #'make-keyword #'string-upcase)
                                 (split-sequence
                                  #\. (subseq action/raw (length #1#))))
                         (split-sequence #\. action/raw))))
        (list* subject action (when kind (list :kind kind)))))))

(assert (equal (parse-permission "GROUP:hudson.model.Item.Read:authenticated")
               '("authenticated" (:item :read) :kind :group)))
(assert (equal (parse-permission "USER:hudson.model.Item.Read:joeuser")
               '("joeuser" (:item :read) :kind :user)))
(assert (equal (parse-permission "USER:something:joeuser")
               '("joeuser" ("something") :kind :user)))
(assert (equal (parse-permission "hudson.model.Item.Read:joeuser")
               '("joeuser" (:item :read))))

(defun unparse-permission (spec)
  (let+ (((subject (&whole action first &rest &ign) &key kind) spec)
         (action (if (keywordp first)
                     (format nil "hudson.model.~{~@(~A~)~^.~}" action)
                     (format nil "~{~A~^.~}" action))))
    (format nil "~@[~A:~]~A:~A" kind action subject)))

(assert (string= (unparse-permission '("authenticated" (:item :read) :kind :group))
                 "GROUP:hudson.model.Item.Read:authenticated"))
(assert (string= (unparse-permission '("joeuser" (:item :read) :kind :user))
                 "USER:hudson.model.Item.Read:joeuser"))
(assert (string= (unparse-permission '("joeuser" ("something") :kind :user))
                 "USER:something:joeuser"))
(assert (string= (unparse-permission '("joeuser" (:item :read)))
                 "hudson.model.Item.Read:joeuser"))

(defmethod xloc:xml-> ((value stp:element)
                       (type  (eql 'access-control-rule))
                       &key &allow-other-keys)
  (xloc:with-locations-r/o (((:val action-and-subject :type 'string) "text()"))
      value
    (parse-permission action-and-subject)))

(defmethod xloc:->xml ((value cons)
                       (dest  stp:element)
                       (type  (eql 'access-control-rule))
                       &key &allow-other-keys)
  (check-type value access-control-rule)
  (xloc:with-locations (((:val action-and-subject :type 'string) "text()"))
      dest
    (setf action-and-subject (unparse-permission value))))
