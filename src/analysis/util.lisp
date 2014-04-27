;;;; util.lisp --- Utilities for analysis module.
;;;;
;;;; Copyright (C) 2013, 2014 Jan Moringen
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

(defun find-files (pattern &key (exclude "\.svn"))
  "TODO(jmoringe): document"
  (let ((candidates (directory pattern)))
    (if exclude
        (remove-if (curry #'ppcre:scan exclude) candidates
                   :key #'namestring)
        candidates)))

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
      ((not (and (typep n 'fixnum) (typep m 'fixnum)))
       (error "~@<Inputs too large.~@:>"))
      ((zerop n)
       m)
      ((zerop m)
       n)
      (t
       ;; We need to store only two columns---the current one that is
       ;; being built and the previous one.
       (let ((col      (make-array (1+ m) :element-type 'fixnum))
             (prev-col (coerce (iota (1+ m)) `(simple-array fixnum (,(1+ m))))))
         ;; Loop across all chars of each string
         #+no (declare (type (simple-array fixnum) col prev-col))
         (dotimes (i n)
           (setf (aref col 0) (1+ i))
           (let ((current
                   (iter (for j :from (max (- i window) 0) :below (min (+ i window) m))
                         (let ((value (min (1+ (aref col j))
                                           (1+ (aref prev-col (1+ j)))
                                           (+ (aref prev-col j)
                                              (if (char-equal (schar str1 i) (schar str2 j)) 0 1)))))
                           (setf (aref col (1+ j)) value)
                           (minimizing value)))))
             (when (> current upper-bound)
               (return-from edit-distance upper-bound)))
           (fill col most-positive-fixnum
                 :start 0 :end (min (max (- i window) 0) m))
           (fill col most-positive-fixnum
                 :start (min (+ i window) (1- m)) :end m)
           (rotatef col prev-col))
         (aref prev-col m))))))

(defun format-git-url (url &optional username password)
  (format nil "~(~A~)://~:[~*~:;~:*~A~@[:~A~]@~]~@[~A~]~@[:~D~]~@[~A~]"
          (puri:uri-scheme url)
          username
          password
          (puri:uri-host url)
          (puri:uri-port url)
          (puri:uri-path url)))

(defun run (spec directory)
  (let ((output (make-string-output-stream)))
    (log:debug "~@<Executing ~S~@:>" spec)
    (handler-case
        (inferior-shell:run `((inferior-shell:>& 2 1) ,@spec)
                            :directory directory
                            :output    output)
      (error ()
        (error "~@<Command~@:_~@:_~
                ~2@T~S~@:_~@:_~
                failed with output:~@:_~@:_~
                ~A~@:>"
               spec (get-output-stream-string output))))))
