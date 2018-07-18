;;;; cache.lisp --- Caching of analysis results.
;;;;
;;;; Copyright (C) 2012-2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.analysis)

;;; Keys

(defun octets->hex-string (octets prefix)
  (format nil "~A:~@[~(~{~2,'0X~}~)~]" prefix (coerce octets 'list)))

(defun sub-directory->key (sub-directory prefix)
  (let* ((octets (when sub-directory
                   (sb-ext:string-to-octets
                    (namestring sub-directory) :external-format :utf-8)))
         (hash   (when octets
                   (ironclad:digest-sequence 'ironclad:sha256 octets))))
    (octets->hex-string hash prefix)))

(defun natures->key (natures prefix)
  (let* ((natures (or natures '(:none)))
         (sorted  (sort (map 'list #'string-downcase natures) #'string<)))
    (format nil "~A:~{~A~^;~}" prefix sorted)))

;;; Cache

(defun cache-restore (cache-directory key)
  (with-simple-restart (continue "~@<Do not use cache results.~@:>")
    (let ((file (merge-pathnames key cache-directory)))
      (log:info "~@<Maybe restoring analysis results in ~A~@:>" file)
      (when (probe-file file)
        (log:info "~@<Restoring analysis results in ~A~@:>" file)
        (let+ (((version . data) (cl-store:restore file)))
          (cond
            ((string= version *cache-version*)
             data)
            (t
             (log:warn "~@<Stored results have been produced by version ~
                        ~A while this is version ~A.~@:>"
                       version *cache-version*)
             nil)))))))

(defun cache-store (cache-directory key results)
  (with-simple-restart (continue "~@<Do not cache results.~@:>")
    (unless (probe-file cache-directory)
      (log:info "~@<Creating non-existent cache directory ~S.~@:>"
                cache-directory)
      (ensure-directories-exist cache-directory))
    (let ((file (merge-pathnames key cache-directory)))
      (log:info "~@<Storing analysis results in ~A~@:>" file)
      (cl-store:store (cons *cache-version* results) file))))

(defun cache-or-compute (cache-directory key thunk)
  (or (when (and cache-directory key)
        (cache-restore cache-directory key))
      (let ((results (funcall thunk)))
        (when (and cache-directory key)
          (cache-store cache-directory key results))
        results)))
