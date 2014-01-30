;;; asdf-artifact-download.lisp ---
;;
;; Copyright (C) 2011, 2012 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This Program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This Program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses>.

(cl:in-package :jenkins.runtime)

(defvar *failed-systems* nil
  "Stores a list of system names that have been requested, but could
not be found.")

(defvar *downloaded-systems* nil
  "Stores a list of pathnames corresponding to asd files found in
previously downloaded artifacts.")

(defun sysdef-download-from-artifact (system-name)
  "This function is intended to be added to
`asdf:*system-definition-search-functions*'."
  (unless (and *jenkins-base-url* *job-name* *job-workspace*)
    (warn "~@<Cannot download artifact for system ~A since at least ~
one Jenkins environment variable is missing.~@:>"
	  system-name)
    (return-from sysdef-download-from-artifact nil))

  (let ((name (asdf:coerce-name system-name)) ;;; TODO(jmoringe): do not ignore version
	(deps (merge-pathnames "upstream/" *job-workspace*)))
    (flet ((find-system ()
	     (find name *downloaded-systems*
		   :key  #'pathname-name
		   :test #'string=)))
      (cond
	;; System is known to be not downloadable.
	((member system-name *failed-systems* :test #'string=)
	 nil)

	;; System has been downloaded and cached.
	((find-system))

	;; Try to download and extract the artifact into the
	;; dependency directory.
	((and
	  #+returns-nil (format t "~&~@<; ~@;loading system from artifact ~A~@:>"
				name)
	  (obtain-project-artifact name deps
				   :label    *dep-label*
				   :suffixes *dep-suffixes*
				   :error?   nil)
	  ;;; TODO(jmoringe, 2011-11-16): reset *failed-systems*? we could discover previously failed systems here
	  (setf *downloaded-systems*
		(directory (merge-pathnames "**/*.asd" deps)))) ;;; TODO(jmoringe): don't scan all downloads again
	 (find-system))

	;; Download failed
	(t
	 (push system-name *failed-systems*)
	 nil)))))

(defun register-artifact-download-function ()
  "TODO(jmoringe): document"
  (alexandria:appendf asdf:*system-definition-search-functions*
		      '(sysdef-download-from-artifact)))
