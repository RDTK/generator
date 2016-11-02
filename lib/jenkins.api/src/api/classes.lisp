;;;; classes.lisp --- Classes used by the api module.
;;;;
;;;; Copyright (C) 2012, 2013, 2014, 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.api)

(defvar *initializing?* nil)

(defclass standard-model-object ()
  ((id       :initarg  :id
             :accessor id
             :documentation
             "")
   (data     :initarg  :data
             :accessor %data
             :documentation
             "")
   (get-func :initarg  :get-func
             :type     function
             :accessor get-func
             :documentation
             "")
   (put-func :initarg  :put-func
             :type     (or null function)
             :accessor put-func
             :initform nil
             :documentation
             ""))
  (:default-initargs
   :id       (missing-required-initarg 'standard-model-object :id)
   :get-func (missing-required-initarg 'standard-model-object :get-func)
   #+no :put-func #+no (missing-required-initarg 'standard-model-object :put-func))
  (:documentation
   "TODO(jmoringe): document"))

(defmethod shared-initialize :around ((instance    standard-model-object)
                                      (slots-names t)
                                      &key)
  (let ((*initializing?* t))
    (call-next-method)))

(defmethod slot-unbound :around ((class     t)
                                 (instance  standard-model-object)
                                 (slot-name t))
  (setf (%data instance) nil)
  (update! instance)
  (if (slot-boundp instance slot-name)
      (slot-value instance slot-name)
      (call-next-method)))

(defmethod (setf closer-mop:slot-value-using-class) :around ((new-value t)
                                                             (class     t)
                                                             (instance  standard-model-object)
                                                             (slot      t))
  (when (and (not (member (closer-mop:slot-definition-name slot) '(id data get-func put-func)))
             (not (slot-boundp instance 'data))
             (not *initializing?*))
    (update! instance))
  (call-next-method))

;; TODO(jmoringe, 2013-02-21): rename root? -> toplevel? ? make option?
(defmacro define-model-class (name () (&rest slots) &body options)
  (let+ (((&flet maybe-version-case (spec &key (transform #'identity))
            (typecase spec
              ((cons (eql :version))
               `(cond
                  ,@(mapcar (lambda+ ((pattern expression))
                              (let ((body (funcall transform expression)))
                                (case pattern
                                  ((t) `(t                          ,body))
                                  (t   `((equal (version) ,pattern) ,body)))))
                            (rest spec))))
              (t (funcall transform spec)))))

         ((&flet+ make-slot-spec ((name
                                   &rest options
                                   &key
                                   (initarg   (make-keyword name))
                                   (type      t)
                                   xpath
                                   (optional? t)
                                   (accessor  name)
                                   &allow-other-keys))
            `(,name
              :initarg  ,initarg
              :type     ,(cond
                           ((eq (getf (rest (ensure-list xpath)) :if-multiple-matches) :all)
                            'list)
                           ((and (listp type) (starts-with-subseq "LIST" (string (first type))))
                            'list)
                           (optional?
                            `(or null ,type))
                           (t
                            type))
              :accessor ,accessor
              ,@(remove-from-plist options :type :xpath :optional?))))

         ((&flet+ make-default-initarg ((name
                                         &key
                                         (initarg   (make-keyword name))
                                         (optional? t)
                                         (initform  :TODO-let-plus      initform-supplied?)
                                         &allow-other-keys))
            (unless (or optional? initform-supplied?)
              `(,initarg (missing-required-initarg ',name ,initarg)))))

         ((&flet+ make-xml->slot ((name
                                   &key
                                   (type      t)
                                   (xpath     (format (cdr '(1)) "./~(~A~)/text()" name)) ; TODO(jmoringe, 2012-12-21): let+ workaround
                                   (optional? t)
                                   &allow-other-keys))
            (when xpath
              `(let ((loc (apply #'xloc:loc value
                                 (append
                                  ,(maybe-version-case
                                    xpath :transform (lambda (x)
                                                       `(list ,@(ensure-list x))) )
                                  ,@(when optional?
                                      '('(:if-no-match :do-nothing)))))))
                 (setf (slot-value type ',name)
                       (xloc:val loc :type ',type))))))

         ((&flet+ make-slot->xml ((name
                                   &key
                                   (type      t)
                                   (xpath     (format (cdr '(1)) "./~(~A~)/text()" name))
                                   (optional? t)
                                   &allow-other-keys))
            (when xpath
              `(let ((loc (apply #'xloc:loc dest
                                 (append
                                  ,(maybe-version-case
                                    xpath :transform (lambda (x)
                                                       `(list ,@(ensure-list x))))
                                  (list :if-no-match (if (and ,optional? (not (slot-value value ',name)))
                                                         :do-nothing
                                                         :create))))))
                 (when (xloc:location-result loc)
                   (setf (xloc:val loc :type ',type)
                         (slot-value value ',name)))))))

         (name-slot    (second (or (find :name-slot options :key #'first)
                                '(:name-slot id))))
         (version-slot (second (find :version-slot options :key #'first)))
         (get-func     (second (find :get-func options :key #'first)))
         (put-func     (second (find :put-func options :key #'first)))
         (root?        (or get-func put-func)))
    `(progn
       (defclass ,name (,@(when root? '(standard-model-object)))
         (,@(mapcar #'make-slot-spec slots))
         (:default-initargs
          ,@(append
             (when get-func `(:get-func ,get-func))
             (when put-func `(:put-func ,put-func))
             (mappend #'make-default-initarg slots)
             (second (find :default-initargs options :key #'first))))
         ,@(remove-if (lambda (key)
                        (member key '(:name-slot :version-slot
                                      :get-func :put-func
                                      :default-initargs)))
                      options :key #'first))

       ;; TODO(jmoringe, 2013-01-03): get rid of this
       (defmethod ,name ((id t) &rest initargs &key &allow-other-keys)
         (apply #'make-instance ',name :id id initargs))

       (defmethod xloc:xml-> ((value stp:element)
                              (type  ,name)
                              &key &allow-other-keys)
         (flet (,@(when version-slot
                    `((version () (,version-slot type)))))
           ,@(when version-slot '((declare (ignorable #'version))))
           ,@(mapcar #'make-xml->slot slots))
         type)

       (defmethod xloc:->xml ((value ,name)
                              (dest  stp:element)
                              (type  (eql ',name))
                              &key &allow-other-keys)
         (flet (,@(when version-slot
                    `((version () (,version-slot value)))))
           ,@(when version-slot '((declare (ignorable #'version))))
           ,@(mapcar #'make-slot->xml slots))
         dest)

       ,@(when root?
           `((defmethod update! ((object ,name))
               (let+ (((&accessors id (data %data) get-func) object))
                 (setf data (funcall get-func id))
                 (xloc:xml-> (stp:root data) object)))

             (defmethod commit! ((object ,name))
               (let+ (((&accessors-r/o id (data %data) put-func) object))
                 (unless put-func
                   (error "~@<Read-only object ~A.~@:>" object))
                 (xloc:->xml object (stp:root data) ',name)
                 (funcall put-func id data))
               object)))

       ,@(when name-slot
           `((defmethod print-object ((object ,name) stream)
               (print-unreadable-object (object stream :type t :identity t)
                 (let* ((value (,name-slot object))
                        (end   (when (stringp value)
                                 (or (position #\Newline value) (length value)))))
                   (princ (if end (subseq value 0 end) value) stream)))))))))

(defmacro define-interface-implementations ((name
                                             &key
                                             (class-location '(xloc:name ".")))
                                            &body implementations)
  (let+ (((class-accessor class-path) class-location)
         (name->class-table (format-symbol *package* "*NAME->~A-CLASS*" name))
         (class->name-table (format-symbol *package* "*CLASS->~A-NAME*" name))
         ((&flet+ make-implementation (((key class &key plugin) (&rest slots) &body options))
            (let ((class-name (format-symbol *package* "~A/~A" name key)))
             `((setf (gethash ,class       ,name->class-table) ',class-name
                     (gethash ',class-name ,class->name-table) ,class)

               (define-model-class ,class-name ()
                   (,@(when plugin
                        `((%plugin :type     string
                                   :xpath    "@plugin"
                                   :initform ,plugin)))
                    ,@slots)
                 ,@options
                 (:version-slot %plugin)))))))
   `(progn
      (deftype ,name () t)

      (defvar ,name->class-table (make-hash-table :test #'equal))
      (defvar ,class->name-table (make-hash-table :test #'eq))

      (defmethod xloc:xml-> ((value stp:element)
                             (type  (eql ',name))
                             &key &allow-other-keys)
        ,(format nil "Lookup the name of the ~S implementation and ~
                      convert VALUE to an instance of that type."
                 name)
        ;; Try to look up the implementation class for the
        ;; implementation name stored in VALUE. If the class cannot be
        ;; found, signal an `unmapped-class' condition and return a
        ;; marker object.
        (let ((name (,class-accessor (xloc:loc value ,class-path))))
          (if-let ((class-name (gethash name ,name->class-table)))
            (xloc:xml-> value class-name)
            (progn
              (signal 'unmapped-class
                      :interface ',name
                      :name      name)
              (list :unimplemented ',name name value)))))

      (defmethod xloc:->xml ((value t)
                             (dest  stp:element)
                             (type  (eql ',name))
                             &key &allow-other-keys)
        ,(format nil "Store the ~S instance VALUE in DEST." name)
        (let* ((class-name (class-name (class-of value)))
               (name       (gethash class-name ,class->name-table)))
          (unless name
            (error ,(format nil "~~@<~~A is not a valid ~S ~
                                 class. Valid ~:*~S classes are ~
                                 ~~{~~S~~^, ~~}.~~@:>"
                            name)
                   value (hash-table-keys ,class->name-table)))

          (setf (,class-accessor (xloc:loc dest ,class-path :if-no-match :create)) name)
          (xloc:->xml value dest class-name)))

      (defmethod xloc:->xml ((value list)
                             (dest  stp:element)
                             (type  (eql ',name))
                             &key &allow-other-keys)
        "This helper method ensures that XML substree DEST is still in
         sync with the XML substree stored in the unmapped
         implementation marker VALUE."
        (check-type value unmapped-marker)
        (assert (eq dest (fourth value))))

      ,@(mappend #'make-implementation implementations))))


;;; `node' class

(define-model-class node ()
  ((name        :type      string)
   (description :type      string)
   (host        :type      string
                :xpath     "launcher/host/text()")
   (mode        :type      keyword)
   (label       :type      (list/space string)
                :xpath     "label/text()")
   (environment :type      tree-map/plist
                :xpath     "/slave/nodeProperties/hudson.slaves.EnvironmentVariablesNodeProperty/envVars/tree-map"))
  (:get-func (lambda (id)      (node-config id)))
  (:put-func (lambda (id data) (setf (node-config id) data))))

(defmethod print-object ((object node) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A ~:[off~;on~]line"
            (name object) (online? object))))

;;; `job' class
;;;
;;; Aggregated classes:
;;;
;;; * `scm'
;;; * `properties'
;;; * `trigger'
;;; * `builder'
;;; * `publisher'

(deftype git-browser ()
  '(member :redmine-web :github-web))

(defmethod xloc:xml-> ((value string)
                       (type  (eql 'git-browser))
                       &key &allow-other-keys)
  (cond
    ((string= value "hudson.plugins.git.browser.RedmineWeb")
     :redmine-web)
    ((string= value "hudson.plugins.git.browser.GithubWeb")
     :github-web)
    (t
     (error "~@<Unknown browser kind ~S.~@:>" value))))

(defmethod xloc:->xml ((value symbol)
                       (dest  (eql 'string))
                       (type  (eql 'git-browser))
                       &key &allow-other-keys)
  (ecase value
    (:redmine-web "hudson.plugins.git.browser.RedmineWeb")
    (:github-web  "hudson.plugins.git.browser.GithubWeb")))

(deftype subversion-checkout-strategy ()
  '(member :fresh-copy :update :emulate-fresh-copy))

(defmethod xloc:xml-> ((value string)
                       (type  (eql 'subversion-checkout-strategy))
                       &key &allow-other-keys)
  (cond
    ((string= value "hudson.scm.subversion.CheckoutUpdater")
     :fresh-copy)
    ((string= value "hudson.scm.subversion.UpdateUpdater")
     :update)
    ((string= value "hudson.scm.subversion.UpdateWithCleanUpdater")
     :emulate-fresh-copy)
    (t
     (error "~@<Unknown checkout strategy ~S.~@:>" value))))

(defmethod xloc:->xml ((value symbol)
                       (dest  (eql 'string))
                       (type  (eql 'subversion-checkout-strategy))
                       &key &allow-other-keys)
  (ecase value
    (:fresh-copy         "hudson.scm.subversion.CheckoutUpdater")
    (:update             "hudson.scm.subversion.UpdateUpdater")
    (:emulate-fresh-copy "hudson.scm.subversion.UpdateWithCleanUpdater")))

(deftype mercurial-revision-type ()
  '(member :branch :tag))

(defmethod xloc:xml-> ((value string)
                       (type  (eql 'mercurial-revision-type))
                       &key &allow-other-keys)
  (switch (value :test #'string=)
    ("BRANCH"
     :branch)
    ("TAG"
     :tag)
    (t
     (error "~@<Unknown revision type ~S.~@:>" value))))

(defmethod xloc:->xml ((value symbol)
                       (dest  (eql 'string))
                       (type  (eql 'mercurial-revision-type))
                       &key &allow-other-keys)
  (princ-to-string value))

(define-interface-implementations (scm
                                   :class-location (xloc:val "@class"))
  ((svn "hudson.scm.SubversionSCM"
        :plugin "subversion@1.43")
   ((url               :type     string
                       :xpath    "locations/hudson.scm.SubversionSCM_-ModuleLocation/remote/text()")
    (credentials       :type     string
                       :xpath    "locations/hudson.scm.SubversionSCM_-ModuleLocation/credentialsId/text()"
                       :initform nil)
    (local-directory   :type     string
                       :xpath    "locations/hudson.scm.SubversionSCM_-ModuleLocation/local/text()"
                       :initform ".")
    (checkout-strategy :type     subversion-checkout-strategy
                       :xpath    "workspaceUpdater/@class"
                       :initform :fresh-copy))
   (:name-slot url))

  ((git "hudson.plugins.git.GitSCM"
        :plugin "git@2.0")
   ((url                    :type     string
                            :xpath    "userRemoteConfigs/hudson.plugins.git.UserRemoteConfig/url/text()")
    (credentials            :type     string
                            :xpath    "userRemoteConfigs/hudson.plugins.git.UserRemoteConfig/credentialsId/text()"
                            :initform nil)
    (branches               :type     (singleton-element "name/text()")
                            :xpath    ("branches/hudson.plugins.git.BranchSpec"
                                       :if-multiple-matches :all))
    (local-branch           :type     string
                            :xpath    (:version
                                       ("git@1.1.26"
                                        "localBranch/text()")
                                       (t
                                        "extensions/hudson.plugins.git.extensions.impl.LocalBranch/localBranch/text()"))
                            :initform nil)
    (clone-timeout          :type     integer
                            :xpath    "extensions/hudson.plugins.git.extensions.impl.CloneOption/timeout/text()"
                            :initform nil)
    (wipe-out-workspace?    :type     (boolean/element "hudson.plugins.git.extensions.impl.WipeWorkspace")
                            :xpath    (:version
                                       ("git@1.1.26"
                                        "wipeOutWorkspace/text()")
                                       (t
                                        "extensions")))
    (clean-before-checkout? :type     (boolean/element "hudson.plugins.git.extensions.impl.CleanBeforeCheckout")
                            :xpath    "extensions")
    (checkout-submodules?   :type     boolean
                            :xpath    (:version
                                       ("git@1.1.26"
                                        "recursiveSubmodules/text()")
                                       (t
                                        "extensions/hudson.plugins.git.extensions.impl.SubmoduleOption/recursiveSubmodules/text()")))
    (shallow?               :type     boolean
                            :xpath    (:version
                                       ("git@1.1.26"
                                        "useShallowClone/text()")
                                       (t
                                        "extensions/hudson.plugins.git.extensions.impl.CloneOption/shallow/text()")))
    (internal-tag?          :type     (boolean/element "hudson.plugins.git.extensions.impl.PerBuildTag")
                            :xpath    (:version
                                       ("git@1.1.26"
                                        "skipTag/text()")
                                       (t
                                        "extensions")))
    (browser-kind           :type     git-browser
                            :xpath    "browser/@class"
                            :initform nil)
    (browser-url            :type     string
                            :xpath    "browser/url/text()"
                            :initform nil))
   (:name-slot url))

  ((bzr "hudson.plugins.bazaar.BazaarSCM")
   ((url :type  string
         :xpath "source/text()"))
   (:name-slot url))

  ((mercurial "hudson.plugins.mercurial.MercurialSCM"
              :plugin "mercurial@1.54")
   ((url           :type     string
                   :xpath    "source/text()")
    (credentials   :type     string
                   :xpath    "credentialsId/text()"
                   :initform nil)
    (revision-type :type     mercurial-revision-type
                   :xpath    "revisionType/text()"
                   :initform :branch)
    (branch        :type     string
                   :xpath    "revision/text()")
    (sub-directory :type     string
                   :xpath    "subdir/text()"
                   :initform nil)
    (clean?        :type     boolean
                   :xpath    "clean/text()"
                   :initform nil)
    (modules       :type     (list/space string)
                   :xpath    ("modules/text()"
                              :if-no-match :create)
                   :initform '()
                   :optional? nil))
   (:name-slot url))

  ((null "hudson.scm.NullSCM")
   ()
   (:name-slot nil)))

(defmethod credentials ((scm scm/null))
  '())

;;; property interface

(deftype cons/parameter-definition ()
  'cons)

(defmethod xloc:xml-> ((value stp:element)
                       (type  (eql 'cons/parameter-definition))
                       &key &allow-other-keys)
  (xloc:with-locations-r/o (((:name class) ".")
                            (name          "name/text()")
                            (description   "description/text()"
                                           :if-no-match :do-nothing)
                            (default       "defaultValue/text()"
                                           :if-no-match :do-nothing)) value
    (list* :kind (cond
                   ((string= class "hudson.model.TextParameterDefinition")   :text)
                   ((string= class "hudson.model.StringParameterDefinition") :string)
                   (t class))
           :name name
           (append
            (when description (list :description description))
            (when default     (list :default default))))))

(defmethod xloc:->xml ((value list)
                       (dest  stp:element)
                       (type  (eql 'cons/parameter-definition))
                       &key &allow-other-keys)
  (xloc:with-locations (((:name class) ".")
                        (name1          "name/text()")
                        (description1   "description/text()")
                        (default1       "defaultValue/text()")) dest
    (let+ (((&key kind name description default) value))
      (setf class (case kind
                    (:text   "hudson.model.TextParameterDefinition")
                    (:string "hudson.model.StringParameterDefinition")
                    (t       kind))
            name1        name
            description1 (or description "")
            default1     (or default ""))))
  dest)

(define-interface-implementations (property)
  ((envinject "EnvInjectJobProperty"
              :plugin "envinject@1.83")
   ((properties :type     (plist/equals list/newline keyword string)
                :xpath    "info/propertiesContent/text()"
                :initform '()))
   (:name-slot nil))

  ((parameters "hudson.model.ParametersDefinitionProperty")
   ((parameters :type     cons/parameter-definition
                :xpath    ("parameterDefinitions/*"
                           :if-multiple-matches :all)
                :initform '()))
   (:name-slot nil)))

;;; trigger interface

(define-interface-implementations (trigger)
  ((scm "hudson.triggers.SCMTrigger")
   ((spec                      :type     string)
    (ignore-post-commit-hooks? :type     boolean
                               :xpath    "ignorePostCommitHooks/text()"
                               :initform nil))
   (:name-slot spec))

  ((timer "hudson.triggers.TimerTrigger")
   ((spec :type string))
   (:name-slot spec))

  ((github "com.cloudbees.jenkins.GitHubPushTrigger"
           :plugin "github@1.4")
   ((spec :type string))
   (:name-slot spec))

  ((reverse "jenkins.triggers.ReverseBuildTrigger")
   ((spec              :type     string ; seems to be unused in Jenkins
                       :xpath    "spec/text()"
                       :initform "")
    (upstream-projects :type     (list/comma string)
                       :xpath    "upstreamProjects/text()"
                       :initform '())
    (threshold         :type     stupid-threshold
                       :xpath    "threshold"
                       :initform :success))
   (:name-slot upstream-projects)))

;;; build-wrapper interface

(define-interface-implementations (build-wrapper)
  ((timeout "hudson.plugins.build__timeout.BuildTimeoutWrapper"
            :plugin "build-timeout@1.11")
   ((kind            :type     (keyword/downcase :absolute)
                     :xpath    "timeoutType/text()"
                     :initform :absolute)
    (timeout/minutes :type     real
                     :xpath    "timeoutMinutes/text()")
    (fail-build?     :type     boolean
                     :xpath   "failBuild/text()"
                     :initform nil))
   (:name-slot kind)))

;;; builder interface

(macrolet
    ((define-maven-settings-type (name default-provider file-path-provider)
       `(progn
          (deftype ,name ()
            '(or (eql :default) string))

          (defmethod xloc:xml-> ((value stp:element)
                                 (type  (eql ',name))
                                 &key &allow-other-keys)
            (xloc:with-locations-r/o (((:@ class) ".")
                                      (path       "path/text()" :if-no-match :do-nothing))
                value
              (cond
                ((string= class ,default-provider)
                 :default)
                ((string= class ,file-path-provider)
                 path))))

          (defmethod xloc:->xml ((value t)
                                 (dest  stp:element)
                                 (type  (eql ',name))
                                 &key &allow-other-keys)
            (xloc:with-locations (((:@ class) ".")
                                  (path       "path/text()"))
                dest
              (etypecase value
                ((eql :default)
                 (setf class ,default-provider)
                 (stp:delete-children dest))
                (string
                 (setf class ,file-path-provider)
                 (setf path value))))
            dest))))

  (define-maven-settings-type maven-settings
    "jenkins.mvn.DefaultSettingsProvider"
    "jenkins.mvn.FilePathSettingsProvider")

  (define-maven-settings-type maven-global-settings
    "jenkins.mvn.DefaultGlobalSettingsProvider"
    "jenkins.mvn.FilePathGlobalSettingsProvider"))

(define-interface-implementations (builder)
  ((shell "hudson.tasks.Shell")
   ((command :type  string))
   (:name-slot command))

  ((batch "hudson.tasks.BatchFile")
   ((command :type  string))
   (:name-slot command))

  ((cmake "hudson.plugins.cmake.CmakeBuilder")
   ((command :type  string
             :xpath "makeCommand/text()"))
   (:name-slot command))

  ((ant "hudson.tasks.Ant"
        :plugin "ant@1.1")
   ((targets    :type  string)
    (properties :type  string))
   (:name-slot targets))

  ((maven "hudson.tasks.Maven")
   ((targets             :type     (list/space string)
                         :initform '())
    (properties          :type     (equals+newline/plist keyword string)
                         :initform '())
    (private-repository? :type     boolean
                         :xpath    "usePrivateRepository/text()"
                         :initform nil)
    (settings            :type     maven-settings
                         :xpath    "settings"
                         :initform :default)
    (global-settings     :type     maven-global-settings
                         :xpath    "globalSettings"
                         :initform :default))
   (:name-slot targets))

  ((copy-artifact "hudson.plugins.copyartifact.CopyArtifact"
                  :plugin "copyartifact@1.27")
   ((project-name :type  string
                  :xpath (:version
                          ("copyartifact@1.25" "projectName/text()")
                          (t                   "project/text()")))
    (filter       :type  string
                  :optional? t)
    (target       :type  string)
    (flatten?     :type  boolean
                  :xpath "flatten/text()")
    ;; TODO(jmoringe, 2012-12-13): temp
    (clazz        :type  string
                  :xpath "selector/@class"))
   (:name-slot project-name)))

;;; publisher interface

(define-model-class warning-parser/file ()
    ((pattern :type  string
              :xpath "pattern/text()")
     (name    :type  string
              :xpath "parserName/text()"))
  (:name-slot pattern))

(define-model-class warning-parser/console ()
    ((name :type  string
           :xpath "parserName/text()"))
  (:name-slot name))

(define-model-class xunit/type ()
    ((kind                      :type     string
                                :xpath    nil)
     (pattern                   :type     string
                                :xpath    "pattern/text()")
     (skip-if-no-test-files?    :type     boolean
                                :xpath    "skipNoTestFiles/text()"
                                :initform nil)
     (fail-if-not-new?          :type     boolean
                                :xpath    "failIfNotNew/text()"
                                :initform t)
     (delete-output-files?      :type     boolean
                                :xpath    "deleteOutputFiles/text()"
                                :initform t)
     (stop-processing-if-error? :type     boolean
                                :xpath    "stopProcessingIfError/text()"
                                :initform t))
  (:name-slot kind))

(defmethod xloc:xml-> :around ((value stp:element)
                               (type  xunit/type)
                               &key &allow-other-keys)
  (let* ((result        (call-next-method))
         (type          (stp:local-name value))
         (type/stripped (if (ends-with-subseq "Type" type)
                            (subseq type 0 (- (length type) 4))
                            type)))
    (setf (slot-value result 'kind) type/stripped)
    result))

(defmethod xloc:->xml :around ((value xunit/type)
                               (dest  stp:element)
                               (type  (eql 'xunit/type))
                               &key &allow-other-keys)
  (let ((result (call-next-method)))
    (setf (stp:local-name result)
          (format nil "~AType" (slot-value value 'kind)))
    result))

(define-interface-implementations (publisher)
  ((ssh "jenkins.plugins.publish__over__ssh.BapSshPublisherPlugin"
        :plugin "publish-over-ssh@1.10")
   ((target           :type    string
                      :xpath   "delegate/publishers/jenkins.plugins.publish__over__ssh.BapSshPublisher/configName/text()")
    (verbose?         :type    boolean
                      :xpath   "delegate/publishers/jenkins.plugins.publish__over__ssh.BapSshPublisher/verbose/text()")
    (source-files     :type    string
                      :xpath   "delegate/publishers/jenkins.plugins.publish__over__ssh.BapSshPublisher/transfers/jenkins.plugins.publish__over__ssh.BapSshTransfer/sourceFiles/text()")
    (excludes         :type    string
                      :xpath   "delegate/publishers/jenkins.plugins.publish__over__ssh.BapSshPublisher/transfers/jenkins.plugins.publish__over__ssh.BapSshTransfer/excludes/text()")
    (remove-prefix    :type    string
                      :xpath   "delegate/publishers/jenkins.plugins.publish__over__ssh.BapSshPublisher/transfers/jenkins.plugins.publish__over__ssh.BapSshTransfer/removePrefix/text()")
    (remote-directory :type    string
                      :xpath   "delegate/publishers/jenkins.plugins.publish__over__ssh.BapSshPublisher/transfers/jenkins.plugins.publish__over__ssh.BapSshTransfer/remoteDirectory/text()"))
   (:name-slot target))

  ((warnings "hudson.plugins.warnings.WarningsPublisher"
             :plugin "warnings@4.21")
   ((encoding        :type      string
                     :xpath     "defaultEncoding/text()"
                     :initform  "UTF-8")
    (file-parsers    :type      warning-parser/file
                     :xpath     ("parserConfigurations/hudson.plugins.warnings.ParserConfiguration"
                                 :if-multiple-matches :all)
                     :optional? nil
                     :initform  '())
    (console-parsers :type      warning-parser/console
                     :xpath     ("consoleParsers/hudson.plugins.warnings.ConsoleParser"
                                 :if-multiple-matches :all)
                     :optional? nil
                     :initform '()))
   (:name-slot nil))

  ((tasks "hudson.plugins.tasks.TasksPublisher"
          :plugin "tasks@4.35")
   ((pattern         :type     (list/comma string)
                     :initform '())
    (exclude         :type     (list/comma string)
                     :xpath    "excludePattern/text()"
                     :initform '())
    (threshold-limit :type     keyword/downcase
                     :xpath    "thresholdLimit/text()"
                     :initform :low)
    (keywords/low    :type     (list/comma string)
                     :xpath    "low/text()"
                     :initform '())
    (keywords/normal :type     (list/comma string)
                     :xpath    "normal/text()"
                     :initform '())
    (keywords/high   :type     (list/comma string)
                     :xpath    "high/text()"
                     :initform '())
    (ignore-case?    :type     boolean
                     :xpath    "ignoreCase/text()"
                     :initform t))
   (:name-slot nil))

  ((archive-artifacts "hudson.tasks.ArtifactArchiver")
   ((files        :type     (list/comma string)
                  :xpath    "artifacts/text()")
    (only-latest? :type boolean
                  :xpath    "onlyLatest/text()"))
   (:name-slot nil))

  ((fingerprint "hudson.tasks.Fingerprinter")
   ((targets          :type      (list/comma string))
    (build-artifacts? :type      boolean
                      :xpath     "recordBuildArtifacts/text()"))
   (:name-slot targets))

  ((sloccount "hudson.plugins.sloccount.SloccountPublisher"
              :plugin "sloccount@1.8")
   ((pattern :type string))
   (:name-slot pattern))

  ((cobertura "hudson.plugins.cobertura.CoberturaPublisher"
              :plugin "cobertura@1.7.1")
   ((report-file :type  string
                 :xpath "coberturaReportFile/text()"))
   (:name-slot report-file))

  ((html "htmlpublisher.HtmlPublisher"
         :plugin "htmlpublisher@1.2")
   ()
   (:name-slot nil))

  ((ci-game "hudson.plugins.cigame.GamePublisher"
            :plugin "ci-game@1.19")
   ()
   (:name-slot nil))

  ((email-notification "hudson.tasks.Mailer"
                       :plugin "mailer@1.16")
   ((recipients            :type     (list/space string))
    (every-unstable-build? :type     boolean
                           :xpath    "dontNotifyEveryUnstableBuild/text()"
                           :initform t)
    (send-to-individuals?  :type     boolean
                           :xpath    "sendToIndividuals/text()"
                           :initform nil))
   (:name-slot recipients))

  ((mailer/extended "hudson.plugins.emailext.ExtendedEmailPublisher"
                    :plugin "email-ext@2.25")
   ()
   (:name-slot nil))

  ((blame-upstream "hudson.plugins.blame__upstream__commiters.BlameUpstreamCommitersPublisher"
                   :plugin "blame-upstream-commiters@1.2")
   ((send-to-individuals? :type  boolean
                          :xpath "sendtoindividuals/text()"))
   (:name-slot nil))

  ((xunit "xunit" :plugin "xunit@1.93")
   ((types :type     xunit/type
           :xpath    ("types/*" :if-multiple-matches :all)
           :initform '()))
   (:name-slot nil))

  ((junit "hudson.tasks.junit.JUnitResultArchiver"
          :plugin "junit@1.4")
   ((pattern             :type     string
                         :xpath    "testResults/text()")
    (keep-long-stdio?    :type     boolean
                         :xpath    "keepLongStdio/text()"
                         :initform nil)
    (health-scale-factor :type     (real 0 1)
                         :xpath    "healthScaleFactor/text()"
                         :initform 1.0))
   (:name-slot pattern)))

(define-model-class job ()
    ((description                :type     string)
     (keep/days                  :type     (or (eql -1) non-negative-integer)
                                 :xpath    "logRotator/daysToKeep/text()")
     (keep/count                 :type     (or (eql -1) non-negative-integer)
                                 :xpath    "logRotator/numToKeep/text()")
     (block-on-downstream-build? :type     boolean
                                 :xpath    "blockBuildWhenDownstreamBuilding/text()")
     (block-on-upstream-build?   :type     boolean
                                 :xpath    "blockBuildWhenUpstreamBuilding/text()")
     (can-roam?                  :type     boolean
                                 :xpath    "canRoam/text()"
                                 :initform t)
     (restrict-to-slaves         :type     string
                                 :xpath    "assignedNode/text()"
                                 :optional? t)
     ;; Interface-based children
     (properties                 :type     property
                                 :xpath    ("properties/*"
                                            :if-multiple-matches :all))
     (triggers                   :type     trigger
                                 :xpath    ("triggers/*"
                                            :if-multiple-matches :all)
                                 ;; TODO :initform '()
                                 )
     (repository                 :type     scm
                                 :xpath    ("scm"))
     (build-wrappers             :type     build-wrapper
                                 :xpath    ("buildWrappers/*"
                                            :if-multiple-matches :all)
                                 ;; TODO :initform '()
                                 )
     (builders                   :type     builder
                                 :xpath    ("builders/*"
                                            :if-multiple-matches :all)
                                 ;; TODO :initform '()
                                 )
     (publishers                 :type     publisher
                                 :xpath    ("publishers/*"
                                            :if-multiple-matches :all)
                                 ;; TODO :initform '()
                                 )

     (dsl :type string :xpath "dsl/text()") ; TODO hack

     ;; TODO Not sure about these
     (slaves          :type     string/node ; TODO(jmoringe, 2012-07-10): not correct
                      :xpath    ("axes/hudson.matrix.LabelAxis[name/text()='label']/values/string"
                                 :if-multiple-matches :all)
                      :optional? t)

     (environment     :type     (equals+newline/plist keyword string)
                      :xpath    "buildWrappers/hudson.plugins.setenv.SetEnvBuildWrapper/localVarText/text()")

     (permissions     :type     access-control-rule
                      :xpath    ("properties/hudson.security.AuthorizationMatrixProperty/permission"
                                 :if-multiple-matches :all))
     (jdk             :type     string
                      :xpath    "jdk/text()")
     ;; TODO these will be moved into the appropriate interfaces
     (redmine-instance :type     string
                       :xpath    #+TODO (:version
                                  ("redmine@0.14"
                                   "properties/hudson.plugins.redmine.RedmineProjectProperty/redmineWebsite/text()")
                                  (t
                                   "properties/hudson.plugins.redmine.RedmineProjectProperty/redmineWebsiteName/text()"))
                                 "properties/hudson.plugins.redmine.RedmineProjectProperty/redmineWebsiteName/text()")
     (redmine-version :type     string
                      :xpath    "properties/hudson.plugins.redmine.RedmineProjectProperty/redmineVersionNumber/text()")
     (redmine-project :type     string
                      :xpath    "properties/hudson.plugins.redmine.RedmineProjectProperty/projectName/text()"))
  (:get-func (lambda (id)      (job-config id)))
  (:put-func (lambda (id data) (setf (job-config id) data))))

(defun job-name-character? (character)
  (or (alphanumericp character) (member character '(#\- #\_ #\.))))

(defun job-name? (name)
  (every #'job-name-character? name))

(deftype job-name ()
  '(satisfies job-name?))

(defun check-job-name (name)
  (unless (job-name? name)
    (let ((offenders (map 'list (lambda (char)
                                  (list char (char-name char)))
                          (remove-duplicates
                           (remove-if #'job-name-character? name)))))
      (error 'simple-type-error
             :datum            name
             :expected-type    'job-name
             :format-control   "~@<Supplied job name ~S contains illegal ~
                                character~P: ~{~{~A (~@[~A~])~}~^, ~}.~@:>"
             :format-arguments (list name (length offenders) offenders)))))

(defmethod initialize-instance :before ((instance job)
                                        &key
                                        id
                                        check-id?)
  (when check-id? (check-job-name id)))

(defmethod kind ((object job))
  (stp:local-name (stp:document-element (%data object))))

(defmethod (setf kind) ((new-value (eql :project))
                        (object    job))
  (setf (kind object) "project"))

(defmethod (setf kind) ((new-value (eql :matrix))
                        (object    job))
  (setf (kind object) '("matrix-project" "matrix-project@1.4")))

(defmethod (setf kind) ((new-value string)
                        (object    job))
  (setf (kind object) (list new-value))
  new-value)

(defmethod (setf kind) ((new-value cons)
                        (object    job))
  (let+ (((local-name &optional plugin) new-value)
         (root (stp:document-element (%data object))))
    (setf (stp:local-name root) local-name)
    (when plugin
      (setf (stp:attribute-value root "plugin") plugin))
    new-value))

(macrolet ((define-of-type-methods (interface
                                    &optional
                                    (plural (symbolicate interface '#:s)))
             (let ((name/list (symbolicate plural    '#:-of-type))
                   (name/one  (symbolicate interface '#:-of-type)))
              `(progn
                 (defmethod ,name/list (type job)
                   (remove-if-not (of-type type) (,plural job)))

                 (defmethod ,name/one (type job)
                   (find-if (of-type type) (,plural job)))))))

  (define-of-type-methods property properties)
  (define-of-type-methods trigger)
  (define-of-type-methods build-wrapper)
  (define-of-type-methods builder)
  (define-of-type-methods publisher))

(defmethod upstream ((object job))
  (when-let ((reverse (trigger-of-type 'trigger/reverse object)))
    (upstream-projects reverse)))

(defmethod (setf upstream) ((new-value list) (object job))
  (let ((reverse (or (trigger-of-type 'trigger/reverse object) ; TODO make a function or macro ensure-...
                     (let ((instance (make-instance 'trigger/reverse)))
                       (appendf (triggers object) (list instance))
                       instance))))
    (setf (upstream-projects reverse) new-value)))

;;; Permissions

(defmethod grant ((job job) (subject string) (action cons))
  (pushnew (list subject action) (permissions job) :test #'string=)
  (permissions job))

(defmethod revoke ((job job) (subject string) (action cons))
  (removef (permissions job) (list  subject action) :test #'string=)
  (permissions job))

(macrolet
    ((define-permission-methods (name)
       `(progn
          (defmethod ,name ((job string) (subject t) (action t))
            (,name (job job) subject action))

          (defmethod ,name ((job job) (subject list) (action t))
            (mapc #'(lambda (subject) (,name job subject action)) subject)
            (permissions job)))))

  (define-permission-methods grant)
  (define-permission-methods revoke))

;;; `build' class

(define-model-class build ()
    ((building?  :type  boolean
                 :xpath "building/text()")
     (slave-name :type  string
                 :xpath "builtOn/text()")
     (result     :type  keyword
                 :xpath "result/text()"))
  (:get-func (lambda (id) (build-config id))))

(defmethod job ((build build) &key &allow-other-keys)
  (job (first (split-sequence #\/ (id build)))))

(defmethod slave ((build build) &key &allow-other-keys)
  (node (slave-name build)))

(defmethod failed? ((build build))
  (eq (result build) :failure))

(defmethod print-object ((object build) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A ~A"
            (id object) (if (building? object)
                            :in-progress
                            (result object)))))

;;; `item' class (Queue items)

(define-model-class item ()
    ((job-name :type  string
               :xpath "task/name/text()"))
  (:get-func (lambda (id) (item-config id))))

(defmethod job ((item item) &key &allow-other-keys)
  (job (job-name item)))

;;; `view' class

(define-model-class view ()
    ((jobs :type  string
           :xpath ("job/name/text()"
                   :if-multiple-matches :all)))
  (:get-func (lambda (id)      (view-config id)))
  (:put-func (lambda (id data) (setf (view-config id) data))))
