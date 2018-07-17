;;;; ros-package.lisp --- Analysis of ROS packages.
;;;;
;;;; Copyright (C) 2013-2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

;;;; Partially based on
;;;; https://github.com/ros-infrastructure/rep/blob/master/rep-0140.rst

(cl:in-package #:jenkins.analysis)

(define-constant +ros-package-name+ "package/name/text()"
  :test #'string=
  :documentation
  "Name of the package. Required.")

(define-constant +ros-package-version+ "package/version/text()"
  :test #'string=
  :documentation
  "Version of the package. Required.

   The format is MAJOR.MINOR.PATCH where each part is numeric only.")

(define-constant +ros-package-description/brief+ "package/description/@brief"
  :test #'string=)

(define-constant +ros-package-description/long+ "package/description/text()"
  :test #'string=
  :documentation
  "Description of the package. Required.

   Can consist of multiple lines and may contain XHTML.")

(define-constant +ros-package-persons+
  "package/*[local-name() = \"author\" or local-name() = \"maintainer\"]"
  :test #'string=)

(define-constant +ros-package-license+ "package/license/text()"
  :test #'string=
  :documentation
  "Name of license for the package, e.g. BSD, GPL, LGPL. Required.")

(define-constant +ros-package-url+ "package/url"
  :test #'string=
  :documentation
  "Uniform Resource Locator for associated website, bug tracker etc.")

(define-constant +ros-package-url/lax+ "package/url[normalize-space(text())]"
  :test #'string=
  :documentation
  "Like `+ros-package-url+' but allow and ignore (nonsensical)
   <url></url> elements.")

(define-constant +ros-package-dependencies+
  "package/*[contains(local-name(), \"depend\")]"
  :test #'string=)

(define-constant +rosjava-messags-group-id+
  "org.ros.rosjava_messages/~A"
  :test #'string=
  :documentation
  "Maven group id to use when representing provided rosjava
   artifacts.")

(defmethod analyze ((directory pathname)
                    (kind      (eql :ros-package))
                    &key)
  (let ((package.xml-info (analyze directory :ros-package.xml))
        (cmake-info       (analyze directory :cmake)))
    `(:requires ,(getf cmake-info :requires)
      ,@(remove-from-plist package.xml-info :requires))))

(defmethod analyze ((directory pathname)
                    (kind      (eql :ros-package.xml))
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
         ((:val persons :type 'cons/person+role) +ros-package-persons+
                                                 :if-multiple-matches :all)
         (license                                +ros-package-license+
                                                 :if-no-match :do-nothing)
         ((:val url :type 'cons/url)             +ros-package-url/lax+
                                                 :if-multiple-matches :all)
         ((:val dependencies :type 'list/depend) +ros-package-dependencies+
                                                 :if-multiple-matches :all))
        document
      (let+ (((&values authors maintainers) (partition-persons persons))
             (version (parse-version version))
             ((&flet trim (string)
                (string-trim '(#\Space #\Tab #\Newline) string))))
        `(:natures  (,:ros-package)
          :provides ((:cmake ,name                    ,version)
                     (:maven ,(rosjava-artifact name) ,version))
          :requires ,(mapcan (lambda+ ((kind name &optional version))
                               (when (member kind '("build" "test")
                                             :test #'string=)
                                 `((:cmake ,name
                                           ,@(when version `(,(parse-version version)))))))
                             dependencies)
          ,@(cond
              (description/long
               `(:description ,(trim description/long)))
              (description/brief
               `(:description ,(trim description/brief))))
          ,@(when url         `(:url         ,(cdr (first url))))
          ,@(when license     `(:license     ,license))
          ,@(when authors     `(:authors     ,authors))
          ,@(when maintainers `(:maintainers ,maintainers)))))))

;;; Utilities

(defun rosjava-artifact (name)
  (format nil +rosjava-messags-group-id+ name))

(defun partition-persons (persons)
  (loop :for (person . role) :in persons
     :when (string= role "author")
     :collect person :into authors
     :when (string= role "maintainer")
     :collect person :into maintainers
     :finally (return (values authors maintainers))))

;;; Conversion helpers

(deftype cons/person+role ()
  '(cons rosetta-project.model.resource:person string))

(defmethod xloc:xml-> ((value stp:element) (type (eql 'cons/person+role))
                       &key
                       inner-types)
  (declare (ignore inner-types))
  (xloc:with-locations-r/o (((:name role) ".")
                            (email        "@email" :if-no-match :do-nothing)
                            (name         "text()"))
      value
    (let+ (((&flet make-email-uri (address)
              (make-instance 'puri:uri
                             :scheme :mailto
                             :path   (string-downcase address)))))
      (cons (apply #'rosetta-project.model.resource:make-person
                   name (when email (list :email (make-email-uri email))))
            role))))

(deftype list/depend ()
  "(PHASE DEPENDENCY &optional MINIMUM-VERSION)

   where PHASE is either t, or the name of a phase."
  '(cons string (cons string (or null (cons string null)))))

(defmethod xloc:xml-> ((value stp:element) (type (eql 'list/depend))
                       &key
                       inner-types)
  (declare (ignore inner-types))
  (xloc:with-locations-r/o
      (((:name name) ".")
       (value        "text()")
       (version>=    "@version_gte" :if-no-match :do-nothing))
      value
    (list (if (string= name "depend")
              t
              (subseq name 0 (- (length name) (length "_depend"))))
          value
          version>=)))

(deftype cons/url ()
  '(cons (or string (eql :default)) string))

(defmethod xloc:xml-> ((value stp:element) (type (eql 'cons/url))
                       &key
                       inner-types)
  (declare (ignore inner-types))
  (xloc:with-locations-r/o (((:@ type) "." :if-no-match :do-nothing)
                            (value     "text()"))
      value
    (cons (or type :default) value)))
