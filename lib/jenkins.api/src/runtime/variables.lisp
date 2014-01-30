;;; variables.lisp ---
;;
;; Copyright (C) 2011 Jan Moringen
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

(in-package :jenkins.runtime)

(defvar *jenkins-base-url*
  (let ((url (sb-posix:getenv "JENKINS_URL")))
    (subseq url 0 (position #\/ url :from-end t)))
  "Base URL of the jenkins instance that has the artifacts.")

(defvar *job-name* (sb-posix:getenv "JOB_NAME")
  "Our job name.")

(defvar *job-workspace* (sb-posix:getenv "WORKSPACE")
  "Our workspace.")

(defvar *dep-suffixes* (append
			(split-sequence:split-sequence
			 #\: (sb-posix:getenv "DEP_SUFFIX")
			 :remove-empty-subseqs t)
			'(nil))
  "A list of priority-sorted suffixes to append to systems when trying
to download artifacts.")

(defvar *dep-label* (sb-posix:getenv "DEP_LABEL")
  "A label that we should splice into artifact URLs.")
