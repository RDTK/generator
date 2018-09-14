;;;; util.lisp --- Utilities for analysis module.
;;;;
;;;; Copyright (C) 2013-2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.analysis)

;;; Retrying

(defun call-with-retry-restart (thunk report)
  (tagbody
   :start
     (restart-case
         (return-from call-with-retry-restart
           (funcall thunk))
       (retry ()
         :report (lambda (stream) (funcall report stream))
         (go :start)))))

(defmacro with-retry-restart ((format-control &rest format-arguments) &body body)
  `(call-with-retry-restart
    (lambda () ,@body)
    (lambda (stream)
      (format stream ,format-control ,@format-arguments))))

(defun call-with-retries (thunk condition-type limit)
  (let ((retry-count 0))
    (handler-bind
        ((condition (lambda (condition)
                      (cond
                        ((not (typep condition condition-type)))
                        ((>= retry-count limit)
                         (error "~@<Giving up after ~D ~
                                 retr~:@P. Original error: ~A~@:>"
                                retry-count condition))
                        (t
                         (when-let ((restart (find-restart 'retry condition)))
                           (incf retry-count)
                           (log:warn "~@<Invoking retry restart ~A, ~
                                      attempt ~D of ~D.~@:>"
                                     restart retry-count limit)
                           (invoke-restart restart)))))))
      (funcall thunk))))

(defmacro with-retries ((condition-type &key limit) &body body)
  `(call-with-retries (lambda () ,@body) ',condition-type ,limit))

;;;

(defun default-temporary-directory (&key
                                    (base #P"/tmp/")
                                    (hint "build-generator"))
  (assert (not (find #\/ hint)))

  (ensure-directories-exist base)
  (let* ((hint-pathname (make-pathname :name hint
                                       :type "XXXXXX"))
         (template      (namestring
                         (if hint
                             (merge-pathnames hint-pathname
                                              (parse-namestring base))
                             (parse-namestring base)))))
    (sb-ext:parse-native-namestring (sb-posix:mkdtemp template)
                                    nil *default-pathname-defaults*
                                    :as-directory t)))

(defun find-files (pattern &key (exclude "(\.svn|\.git)"))
  (let ((candidates (directory pattern)))
    (if exclude
        (remove-if (curry #'ppcre:scan exclude) candidates
                   :key #'namestring)
        candidates)))

(defun make-file-generator (directory patterns)
  (let+ ((patterns patterns)
         (files    nil)
         ((&labels next ()
            (cond
              (files
               (pop files))
              (patterns
               (let ((pattern (merge-pathnames (pop patterns) directory)))
                 (setf files (directory pattern))
                 (next)))))))
    #'next))

(defun safe-external-format-argument ()
  #+sbcl '(:external-format (:utf-8 :replacement #\?)))

(defun read-file-into-string* (filename &rest args &key external-format)
  (apply #'read-file-into-string filename
         (append (or external-format (safe-external-format-argument))
                 (remove-from-plist args :external-format))))

(declaim (ftype (function (string string
                                  &key
                                  (:window array-index)
                                  (:upper-bound array-index))
                          (values array-index &optional))
                edit-distance))
(defun edit-distance (string1 string2
                      &key
                      (upper-bound 2000)
                      (window      (truncate upper-bound (/ 1.2))))
  "Return the Levenshtein distance between STRING1 and STRING2.

   Source: Wikibook
   http://en.wikibooks.org/wiki/Algorithm_Implementation/Strings/Levenshtein_distance#Common_Lisp"
  (declare (optimize speed))
  (let ((n (length string1))
        (m (length string2)))
    ;; Check trivial cases
    (cond
      ((zerop n)
       m)
      ((zerop m)
       n)
      (t
       ;; We need to store only two columns---the current one that is
       ;; being built and the previous one.
       (let ((col      (make-array (1+ m) :element-type 'array-index))
             (prev-col (make-array (1+ m)
                                   :element-type     'array-index
                                   :initial-contents (iota (1+ m)))))
         (declare (type (simple-array array-index 1) col prev-col))
         ;; Loop across all chars of each string
         (dotimes (i n)
           (setf (aref col 0) (1+ i))
           (let ((current
                  (iter (for (the (or (eql -1) array-index) j)
                             :from (max (- i window) 0)
                             :below (min (+ i window) m))
                        (let ((value (min (1+ (aref col j))
                                          (1+ (aref prev-col (1+ j)))
                                          (+ (aref prev-col j)
                                             (if (char= (schar string1 i)
                                                        (schar string2 j))
                                                 0
                                                 1)))))
                          (setf (aref col (1+ j)) value)
                          (minimizing (the array-index value))))))
             (declare (type array-index current))
             (when (> current upper-bound)
               (return-from edit-distance upper-bound)))
           (fill col (- array-dimension-limit 2)
                 :start 0 :end (min (max (- i window) 0) (1+ m)))
           (fill col (- array-dimension-limit 2)
                 :start (min (+ i window 1) (1+ m)) :end (1+ m))
           (rotatef col prev-col))
         (aref prev-col (min m (+ n window -1))))))))

(defun ensure-directory-uri (uri)
  (let ((result (puri:copy-uri uri)))
    (when-let ((path (puri:uri-path result)))
      (unless (ends-with #\/ path)
        (setf (puri:uri-path result)
              (concatenate 'string path "/"))))
    result))

(defun format-git-url (url &optional username password)
  (format nil "~(~A~)://~:[~*~:;~:*~A~@[:~A~]@~]~@[~A~]~@[:~D~]~@[~A~]"
          (puri:uri-scheme url)
          username
          password
          (puri:uri-host url)
          (puri:uri-port url)
          (puri:uri-path url)))

(defun run (spec directory
            &key
            (environment '() environment-supplied?))
  (log:debug "~@<Executing ~S~@:>" spec)
  (let+ ((output (make-string-output-stream))
         ((&values &ign &ign code)
          (handler-case
              (apply #'inferior-shell:run/nil `((inferior-shell:>& 2 1) ,@spec)
                     :directory directory
                     :output    output
                     :on-error  nil
                     (when environment-supplied?
                       (list :environment environment)))
            (error (condition)
              (error "~@<Error executing command~@:_~@:_~
                      ~2@T~/jenkins.analysis::%print-process-spec/~@:_~@:_~
                      ~A~@:>"
                     spec condition)))))
    (unless (zerop code)
      (error "~@<Command~@:_~@:_~
              ~2@T~/jenkins.analysis::%print-process-spec/~@:_~@:_~
              failed with exit code ~D and output:~@:_~@:_~
              ~A~@:>"
             spec code (get-output-stream-string output)))
    (get-output-stream-string output)))

(defun %print-process-spec (stream spec &optional colon? at?)
  (declare (ignore colon? at?))
  (inferior-shell:print-process-spec spec stream))

;;; Utilities for persons

(defun parse-people-list (thing)
  (let ((repository (make-instance 'rs.m.d::base-repository)))
    (rs.f:process `(:person-list :identity-score-limit :non-functional)
                  thing
                  `(:model :repository ,repository))))

(defun merge-people-list (list)
  (values (rosetta-project.model.resource:merge-persons! list)))

(defun parse-and-merge-people-list (thing)
  (merge-people-list (parse-people-list thing)))

(defun make-names->person-list (&key count)
  (let ((names+counts (make-hash-table :test #'equal)))
    (lambda (&optional name)
      (if name
          (incf (gethash name names+counts 0))
          (names+counts->person-list names+counts :count count)))))

(defun names+counts->person-list (names+counts &key count)
  ;; We receive a mapping from names to counts. We map the names to
  ;; person instances and merge those, which may result in duplicates
  ;; in PERSONS.
  (let* ((names+counts/list (hash-table-alist names+counts))
         (persons           (nth-value
                             1 (rosetta-project.model.resource:merge-persons!
                                (parse-people-list
                                 (map 'list #'car names+counts/list)))))
         (persons+counts    (make-hash-table :test #'eq)))
    ;; To handle duplicates in PERSONS, we do another pass,
    ;; accumulating counts by person in PERSONS+COUNTS.
    (map nil (lambda+ (person (&ign . count))
               (incf (gethash person persons+counts 0) count))
         persons names+counts/list)
    ;; Sort and potentially truncate the final list, then return only
    ;; the `person' instances.
    (let ((sorted (sort (hash-table-alist persons+counts) #'> :key #'cdr)))
      (map 'list #'car
           (if count
               (subseq sorted 0 (min (length sorted) count))
               sorted)))))
