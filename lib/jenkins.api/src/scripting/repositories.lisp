;;;; repositories.lisp ---
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.scripting)

(defvar *template* "<scm class=\"hudson.plugins.git.GitSCM\">
    <configVersion>2</configVersion>
    <userRemoteConfigs>
      <hudson.plugins.git.UserRemoteConfig>
        <name/>
        <refspec/>
        <url>https://redmine.amarsi-project.eu/git/quaddrivers.git</url>
      </hudson.plugins.git.UserRemoteConfig>
    </userRemoteConfigs>
    <branches>
      <hudson.plugins.git.BranchSpec>
        <name>**</name>
      </hudson.plugins.git.BranchSpec>
    </branches>
    <disableSubmodules>false</disableSubmodules>
    <recursiveSubmodules>false</recursiveSubmodules>
    <doGenerateSubmoduleConfigurations>false</doGenerateSubmoduleConfigurations>
    <authorOrCommitter>false</authorOrCommitter>
    <clean>false</clean>
    <wipeOutWorkspace>true</wipeOutWorkspace>
    <pruneBranches>false</pruneBranches>
    <remotePoll>false</remotePoll>
    <ignoreNotifyCommit>false</ignoreNotifyCommit>
    <buildChooser class=\"hudson.plugins.git.util.DefaultBuildChooser\"/>
    <gitTool>Default</gitTool>
    <submoduleCfg class=\"list\"/>
    <relativeTargetDir/>
    <reference/>
    <excludedRegions/>
    <excludedUsers/>
    <gitConfigName/>
    <gitConfigEmail/>
    <skipTag>false</skipTag>
    <includedRegions/>
    <scmName/>
  </scm>")

(when nil
 (dolist (job (all-jobs "^rsc-"))
   (when (typep (first (repositories job)) 'scm/svn)
     (let ((old-url (url (first (repositories job)))))
       (setf (repositories job) nil)
       (xloc:->xml job (stp:root (%data job)) 'job)
       (stp:append-child (stp:first-child (%data job))
                         (stp:copy (stp:first-child (cxml:parse *template* (stp:make-builder)))))
       (xloc:xml-> (stp:root (%data job)) job)
       (ppcre:register-groups-bind (trunk branch submodule)
           ("svn/rsc[^/]*/(?:(trunk)|branches/([^/]*))/(.*)" old-url)
         (declare (ignore trunk))
         (setf (url (first (repositories job)))
               (format nil "https://code.cor-lab.org/git/rsc.git~@[.~{~A~^-~}~]"
                       (remove-if (lambda (d) (member d '("rsc")
                                                      :test #'string=))
                                  (reverse (split-sequence #\/ submodule)))))
         (setf (branches (first (repositories job)))
               (if branch
                   (list (format nil "remotes/origin/~A" branch))
                   (list "remotes/origin/master"))))
       (xloc:->xml job (stp:root (%data job)) 'job)
       (format t "~A~%~2@T~S~%->~S ~S~%"
               (id job)
               old-url
               (url (first (repositories job)))
               (branches (first (repositories job))))
       (commit! job)))))

(dolist (job (all-jobs "^rsbag-"))
  (when (typep (first (repositories job)) 'scm/svn)
    (let ((old-url (url (first (repositories job)))))
      (setf (repositories job) nil)
      (xloc:->xml job (stp:root (jenkins.management::%data job)) 'job)
      (stp:append-child (stp:first-child (jenkins.management::%data job))
                        (stp:copy (stp:first-child (cxml:parse *template* (stp:make-builder)))))
      (xloc:xml-> (stp:root (jenkins.management::%data job)) job)
      (ppcre:register-groups-bind (trunk branch submodule)
          ("svn/rsbag[^/]*/(?:(trunk)|branches/([^/]*))(?:/(.*))?" old-url)
        (declare (ignore trunk))
        (setf (url (first (repositories job)))
              (format nil "https://code.cor-lab.org/git/rsbag.git~@[.~{~A~^-~}~]"
                      (remove-if #'emptyp
                       (mapcar (lambda (d) (ppcre:regex-replace "^cl-(?:rsbag)?(?:-)?" d ""))
                               (reverse (split-sequence #\/ (or submodule "manual")))))))
        (setf (branches (first (repositories job)))
              (if branch
                  (list (format nil "remotes/origin/~A" branch))
                  (list "remotes/origin/master")))
        (setf (wipe-out-workspace? (first (repositories job))) t))
      (xloc:->xml job (stp:root (jenkins.management::%data job)) 'job)
      (format t "~A~%~2@T~S~%->~S ~S~%"
              (id job)
              old-url
              (url (first (repositories job)))
              (branches (first (repositories job))))
      (commit! job))))

(when nil
  (dolist (job (all-jobs "^rsb-"))
   (when (and (not (search "time-sync" (id job)))
              (not (search "gstreamer" (id job)))
              (not (search "manual" (id job)))
              (not (search "xml" (id job)))
              (typep (first (repositories job)) 'scm/svn))
     (let ((old-url (url (first (repositories job)))))
       (setf (repositories job) nil)
       (xloc:->xml job (stp:root (%data job)) 'job)
       (stp:append-child (stp:first-child (%data job))
                         (stp:copy (stp:first-child (cxml:parse *template* (stp:make-builder)))))
       (xloc:xml-> (stp:root (%data job)) job)
       (ppcre:register-groups-bind (trunk branch submodule)
           ("svn/rsb[^/]*/(?:(trunk)|branches/([^/]*))/(.*)" old-url)
         (declare (ignore trunk))
         (setf (url (first (repositories job)))
               (format nil "https://code.cor-lab.org/git/rsb.git.~{~A~^-~}"
                       (mapcar (lambda (d) (ppcre:regex-replace "^cl-rsb-" d ""))
                               (remove-if (lambda (d) (member d '("core" "cl-rsb")
                                                              :test #'string=))
                                          (reverse (split-sequence #\/ submodule))))))
         (setf (branches (first (repositories job)))
               (if branch
                   (list (format nil "remotes/origin/~A" branch))
                   (list "remotes/origin/master"))))
       (xloc:->xml job (stp:root (%data job)) 'job)
       (format t "~A~%~2@T~S~%->~S ~S~%"
               (id job)
               old-url
               (url (first (repositories job)))
               (branches (first (repositories job)))))
     (commit! job))))


(when nil
  (dolist (job (all-jobs "^rsb-xml"))
    (when (and (typep (first (repositories job)) 'scm/svn)
               (not (string= (id job) "rsb-xml-java-0.4")))
      (let ((old-url (url (first (repositories job)))))
        (setf (repositories job) nil)
        (xloc:->xml job (stp:root (jenkins.management::%data job)) 'job)
        (stp:append-child (stp:first-child (jenkins.management::%data job))
                          (stp:copy (stp:first-child (cxml:parse *template* (stp:make-builder)))))
        (xloc:xml-> (stp:root (jenkins.management::%data job)) job)
        (ppcre:register-groups-bind (trunk branch submodule)
            ("svn/rsb-xml[^/]*/(?:(trunk)|branches/([^/]*))/(.*)" old-url)
          (declare (ignore trunk))
          (setf (url (first (repositories job)))
                (format nil "https://code.cor-lab.org/git/rsb.git.~{~A~^-~}"
                        (mapcar (lambda (d) (ppcre:regex-replace "^cl-rsb-xml" d ""))
                                (remove-if (lambda (d) (member d '("core" "cl-rsb-xml")
                                                               :test #'string=))
                                           (reverse (split-sequence #\/ submodule))))))
          (setf (branches (first (repositories job)))
                (if branch
                    (list (format nil "remotes/origin/~A" branch))
                    (list "remotes/origin/master"))))
        (xloc:->xml job (stp:root (jenkins.management::%data job)) 'job)
        (format t "~A~%~2@T~S~%->~S ~S~%"
                (id job)
                old-url
                (url (first (repositories job)))
                (branches (first (repositories job)))))
      (commit! job))))

(when nil
  (dolist (job (all-jobs "rsc"))
    (let ((changed? nil))
      (dolist (repo (repositories job))
        (when (and (typep repo 'scm/git)
                   (not (skip-internal-tag? repo)))
          (setf (skip-internal-tag? repo) t
                changed?                  t)))
      (when changed?
        (commit! job)))))
