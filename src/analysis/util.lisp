;;;; util.lisp --- Utilities for analysis module.
;;;;
;;;; Copyright (C) 2013, 2014, 2015, 2016, 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.analysis)

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
(defun edit-distance (str1 str2
                      &key
                      (window      1000)
                      (upper-bound 200))
  "Calculates the Levenshtein distance between STR1 and STR2, returns
   an editing distance (int).

   Source: Wikibook
   http://en.wikibooks.org/wiki/Algorithm_Implementation/Strings/Levenshtein_distance#Common_Lisp"
  (let ((n (length str1))
        (m (length str2)))
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
                                   :element-type     '(unsigned-byte 62)
                                   :initial-contents (iota (1+ m)))))
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
                                              (if (char-equal (schar str1 i) (schar str2 j))
                                                  0
                                                  1)))))
                           (setf (aref col (1+ j)) value)
                           (minimizing value)))))
             (declare (type array-index current))
             (when (> current upper-bound)
               (return-from edit-distance upper-bound)))
           (fill col (1- array-dimension-limit)
                 :start 0 :end (min (max (- i window) 0) m))
           (fill col (1- array-dimension-limit)
                 :start (min (+ i window) (1- m)) :end m)
           (rotatef col prev-col))
         (aref prev-col m))))))

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

;;; Utilities for dependencies

(defun effective-requires (requires provides)
  (set-difference
   requires provides
   :test (lambda+ ((required-nature required-name &optional required-version)
                   (provided-nature provided-name &optional provided-version))
           (and (eq              required-nature  provided-nature)
                (string=         required-name    provided-name)
                (version-matches required-version provided-version)))))

(defun merge-dependencies (dependencies &key (test #'version>=))
  (let ((seen (make-hash-table :test #'equal)))
    (iter (for dependency in dependencies)
          (let+ (((nature name &optional version) dependency)
                 (key (cons nature name)))
            (setf (gethash key seen)
                  (let ((current (gethash key seen)))
                    (if (or (not current)
                            (funcall test version (third current)))
                        dependency
                        current)))))
    (hash-table-values seen)))

;;; Utilities for persons

(defun parse-people-list (thing)
  (values
   (rosetta-project.model.resource:merge-persons!
    (let ((repository (make-instance 'rs.m.d::base-repository)))
      (rs.f:process :person-list thing `(:model :repository ,repository))))))

(defun make-names->person-list (&key count)
  (let ((names+counts (make-hash-table :test #'equal)))
    (lambda (&optional name)
      (if name
          (incf (gethash name names+counts 0))
          (names+counts->person-list names+counts :count count)))))

(defun names+counts->person-list (names+counts &key count)
  (let* ((sorted    (sort (hash-table-alist names+counts) #'> :key #'cdr))
         (truncated (if count
                        (subseq sorted 0 (min (length sorted) count))
                        sorted)))
    (parse-people-list (map 'list #'car truncated))))
