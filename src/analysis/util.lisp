;;;; util.lisp --- Utilities for analysis module.
;;;;
;;;; Copyright (C) 2013-2022 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:build-generator.analysis)

;;; URIs

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

;;; Pathnames

(defun check-repository-sub-directory (repository-directory
                                       sub-directory)
  (let ((directory (merge-pathnames sub-directory repository-directory)))
    (unless (probe-file directory)
      (error "~@<The specified sub-directory \"~A\" does not exist ~
              in the repository.~@:>"
             sub-directory))
    directory))

;;; Sub-processes

(defun run (spec directory
            &key (environment '() environment-supplied?))
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
                      ~2@T[in directory ~A]~@:_~
                      ~2@T~/build-generator.analysis::%print-process-spec/~@:_~@:_~
                      ~A~@:>"
                     directory spec condition)))))
    (unless (zerop code)
      (error "~@<Command~@:_~@:_~
              ~2@T[in directory ~A]~@:_~
              ~2@T~/build-generator.analysis::%print-process-spec/~@:_~@:_~
              failed with exit code ~D and output:~@:_~@:_~
              ~A~@:>"
             directory spec code (get-output-stream-string output)))
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
