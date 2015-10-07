;;;; ros-package.lisp --- Analysis of ROS packages.
;;;;
;;;; Copyright (C) 2013, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.analysis)

(define-constant +ros-package-name+ "package/name/text()"
  :test #'string=)

(define-constant +ros-package-version+ "package/version/text()"
  :test #'string=)

(define-constant +ros-package-description/brief+ "package/description/@brief"
  :test #'string=)

(define-constant +ros-package-description/long+ "package/description/text()"
  :test #'string=)

(define-constant +ros-package-persons+
  "package/*[local-name() = \"author\" or local-name() = \"maintainer\"]"
  :test #'string=)

(define-constant +ros-package-license+ "package/license/text()"
  :test #'string=)

(define-constant +ros-package-url+ "package/url"
  :test #'string=)

(define-constant +ros-package-dependencies+
  "package/*[contains(local-name(), \"depend\")]"
  :test #'string=)

(defmethod analyze ((directory pathname)
                    (kind      (eql :ros-package))
                    &key)
  (let* ((package-file (merge-pathnames "package.xml" directory))
         (document     (cxml:parse package-file (stp:make-builder))))
    (xloc:with-locations-r/o
        ((name                                   +ros-package-name+)
         (version                                +ros-package-version+)
         (description/brief                      +ros-package-description/brief+
                                                 :if-no-match :do-nothing)
         (description/long                       +ros-package-description/long+
                                                 :if-no-match :do-nothing)
         ((:val persons :type 'plist/person)     +ros-package-persons+
                                                 :if-multiple-matches :all)
         (license                                +ros-package-license+
                                                 :if-no-match :do-nothing)
         ((:val url :type 'cons/url)             +ros-package-url+
                                                 :if-multiple-matches :all)
         ((:val dependencies :type 'list/depend) +ros-package-dependencies+
                                                 :if-multiple-matches :all))
        document
      (append `(:versions ((:main ,version))
                :provides ((:ros-package ,name))
                :requires ,(mapcan (lambda+ ((kind name))
                                     (when (string= kind "build")
                                       `((:ros-package ,name))))
                                   dependencies))
              (cond
                (description/long  `(:description ,description/long))
                (description/brief `(:description ,description/brief)))
              (when url     `(:url ,(cdr (first url))))
              (when license `(:license ,license))
              (when persons `(:authors ,(mapcar (lambda+ ((name &key role email))
                                                  (declare (ignore role email))
                                                  (format nil "~A~@[ <~A>~]" name email))
                                                persons)))))))

;;; Conversion helpers

(deftype plist/person ()
  '(cons string))

(defmethod xloc:xml-> ((value stp:element) (type (eql 'plist/person))
                       &key
                       inner-types)
  (declare (ignore inner-types))
  (xloc:with-locations-r/o (((:name role) ".")
                            (email        "@email" :if-no-match :do-nothing)
                            (name         "text()"))
      value
    (list* name :role role (when email (list :email email)))))

(deftype list/depend ()
  '(cons string (cons string null)))

(defmethod xloc:xml-> ((value stp:element) (type (eql 'list/depend))
                       &key
                       inner-types)
  (declare (ignore inner-types))
  (xloc:with-locations-r/o (((:name name) ".")
                            (value        "text()"))
      value
    (list (subseq name 0 (- (length name) (length "_depend")))
          value)))

(deftype cons/url ()
  '(cons string string))

(defmethod xloc:xml-> ((value stp:element) (type (eql 'cons/url))
                       &key
                       inner-types)
  (declare (ignore inner-types))
  (xloc:with-locations-r/o (((:@ type) ".")
                            (value     "text()"))
      value
    (cons type value)))
