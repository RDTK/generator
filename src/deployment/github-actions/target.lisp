;;;; target.lisp --- Target definition for generating a GitHub Actions.
;;;;
;;;; Copyright (C) 2020, 2022 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:build-generator.deployment.github-actions)

;;; `github-actions-target'

(defclass github-actions-target ()
  ((output-directory :initarg :output-directory
                     :type    pathname ; TODO directory-pathname
                     :reader  output-directory
                     :documentation
                     #.(format nil "The directory into which the ~
                        GitHub Actions workflow definition(s) should ~
                        be written."))
   (platforms        :initarg :platforms
                     :type    (or null (cons string list))
                     :reader  platforms
                     :documentation
                     #.(format nil "A list of platforms for which ~
                        workflows should be generated.")))
  (:default-initargs
   :output-directory (missing-required-initarg 'github-actions-target :output-directory)
   :platforms        (missing-required-initarg 'github-actions-target :platforms))
  (:documentation
   "Write Github Actions workflows that build the specified projects."))

(service-provider:register-provider/class
 'deploy:target :github-actions :class 'github-actions-target)

;;; `platform-target'
;;;
;;; An instance describes one platform and the corresponding GitHub
;;; Actions runner for which a workflow should be generated.

(defclass platform-target ()
  ((%parent      :initarg :parent
                 :reader  parent)
   (%platform    :initarg :platform
                 :reader  platform)
   (%runner-name :initarg :runner-name
                 :reader  runner-name)))

;;;

(defmethod aspects::step-constraints ((aspect aspects::aspect-builder-defining-mixin)
                                      (phase  (eql 'aspects::build))
                                      (step   step))
  (when-let ((builder-class (builder-class step)))
    (let* ((variable        (let ((*package* (find-package '#:keyword)))
                              (symbolicate  '#:aspect.builder-constraints.
                                            builder-class)))
           (constraints/raw (var:value aspect variable nil))
           (constraints     (mapcar #'aspects::parse-constraint constraints/raw)))
      (log:debug "~@<Constraints for ~A in ~A~:@_~
                  ~/aspects::format-constraints/~@:>"
                 step variable constraints)
      constraints)))

;;;

(defun translate-platform (label)
  (let ((parts (split-sequence:split-sequence #\Space label)))
    (optima:match parts
      ;; Linux
      ('("ubuntu"  "latest")   (values parts "ubuntu-latest"))
      ('("ubuntu"  "jammy")    (values parts "ubuntu-22.04"))
      ('("ubuntu"  "focal")    (values parts "ubuntu-20.04"))
      ('("ubuntu"  "bionic")   (values parts "ubuntu-18.04")) ; deprecated
      ('("ubuntu"  "xenial")   (values parts "ubuntu-16.04")) ; deprecated
      ;; MacOS
      ('("macos"   "latest")   (values parts "macos-latest"))
      ('("macos"   "monterey") (values parts "macos-12"))
      ('("macos"   "big-sur")  (values parts "macos-11"))
      ('("macos"   "catalina") (values parts "macos-10.15")) ; deprecated
      ;; Windows
      ('("windows" "latest")   (values parts "windows-latest"))
      ('("windows" "2022")     (values parts "windows-2022"))
      ('("windows" "2019")     (values parts "windows-2019"))
      (otherwise               (error "Unknown platform: ~S" label)))))

(defmethod deploy:deploy ((thing sequence) (target github-actions-target))
  (unless (every (of-type 'project:distribution) thing)
    (return-from deploy:deploy (call-next-method)))

  ;; Make a set of jobs for each target platform in TARGET.
  (let ((output-directory (output-directory target))
        (result           '()))
    (map nil (lambda (platform)
               (with-simple-restart (continue "Skip platform ~A" platform)
                 (let+ (((&values platform runner-name)
                         (translate-platform platform)) ; TODO do this translation when constructing the target?
                        (platform-target (make-instance 'platform-target
                                                        :parent      target
                                                        :platform    platform
                                                        :runner-name runner-name))
                        (jobs            (deploy:deploy thing platform-target)))
                   (map nil (lambda (job)
                              (setf (needs job)
                                    (map 'list (lambda (dependency)
                                                 (find-if (rcurry #'member jobs :test #'eq)
                                                          (model:implementations dependency)))
                                         (model:direct-dependencies
                                          (model:specification job)))))
                        jobs)
                   (push (generate-workflow output-directory jobs platform)
                         result))))
         (platforms target))
    ;; Return a list of created workflows
    result))

;;; TODO separate generating and writing
(defun generate-workflow (output-directory jobs platform)
  (let* ((pretty-name (format nil "~{~@(~A~)~^ ~}" platform))
         (name        (format nil "~{~A~^-~}" platform))
         (filename    (merge-pathnames
                       (make-pathname :name      name
                                      :type      "yml"
                                      :directory '(:relative ".github" "workflows"))
                       output-directory)))
    (with-output-to-file (stream filename :if-exists :supersede)
      (let ((workflow (make-instance 'workflow :name pretty-name
                                               :jobs jobs)))
        (pprint-logical-block (stream nil)
          (output workflow stream))
        workflow))))

(defun package-installation-step (project platform)
  (let* ((requirements (project:platform-requires project platform))
         (apt-command  "DEBIAN_FRONTEND=noninteractive sudo apt-get -qq"))
    (when requirements
      ;; TODO should we update? should we upgrade?
      ;; ~4T&& ~:*~A --assume-yes upgrade \\~@
      (command-step "install-platform-dependencies"
                    (format nil "~A update \\~@
                                 ~4T&& ~:*~A --assume-yes install \\~@
                                 ~6@T~{~<~T\\~%~6@T~1,:;~A~>~^ ~}~
                                 ~2%"
                            apt-command requirements)))))

(defmethod deploy:deploy ((thing project::job) (target platform-target))
  ;; Make and return a GitHub Actions job for the job THING that will
  ;; run on the platform (that is mainly a particular GitHub Actions
  ;; runner) described by TARGET.
  (let+ (((&accessors-r/o runner-name platform) target)
         (name   (substitute #\_ #\. (deploy:job-full-name thing :separator "_")))
         (output (make-instance 'job :name          name
                                     :specification thing
                                     :runs-on       runner-name)))
    (push output (model:implementations thing))

    ;; Apply aspects, respecting declared ordering, and sort generated
    ;; builders according to declared ordering.
    (aspects:extend! (aspects:aspects thing) thing output :github-actions)

    ;; Add a step which installs the platform requirements. This must
    ;; be the first step.
    (when-let ((step (package-installation-step (model:parent thing) platform)))
      (push step (steps output)))

    output))
