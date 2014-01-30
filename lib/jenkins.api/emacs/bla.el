;;; jenkins.el ---
;;
;; Copyright (C) 2012 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;; Keywords: continuousintegration
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


;;; Commentary:
;;


;;; History:
;;


;;; Code:
;;

(defun jenkins-read-job (prompt &optional filter)
  ""
  (let ((names (slime-eval `(cl:mapcar
			     (cl:function api.jenkins.model:id)
			     (cl:remove-if-not
			      ,(if filter filter '(cl:constantly t))
			      (api.jenkins.model:all-jobs))))))
    (when names
      (completing-read prompt names))))


(defun jenkins-read-node (prompt &optional filter)
  ""
  (let ((names (slime-eval `(cl:mapcar
			     (cl:function api.jenkins.model:id)
			     (cl:remove-if-not
			      ,(if filter filter '(cl:constantly t))
			      (api.jenkins.model:all-nodes))))))
    (when names
      (completing-read prompt names))))

(defun jenkins-node-mark-online (name)
  (interactive (list (or (jenkins-read-node
			  "Mark node online: "
			  '(cl:complement (cl:function api.jenkins.model:online?)))
			 (error "All nodes are online"))))
  )

(defun jenkins-node-mark-offline (name)
  (interactive (list (jenkins-read-node
		      "Mark node offline: "
		      '(cl:function api.jenkins.model:online?))))
  )

(defun jenkins-job-copy (source-name new-name)
  ""
  (interactive (let ((source-name (jenkins-read-job "Copy job: ")))
		 (list source-name
		       (read-string "Name of new job: " source-name))))
  (list source-name new-name))

(defun jenkins-job-enable (name)
  (interactive (list (jenkins-read-job "Enable job: " 'api.jenkins.model:enabled?)))
  )



(provide 'jenkins)
;;; jenkins.el ends here
