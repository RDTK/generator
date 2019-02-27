;;;; cache.lisp --- Caching of analysis results.
;;;;
;;;; Copyright (C) 2012-2019 Jan Moringen
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

(defun ensure-cache-directory (cache-directory)
  (unless (probe-file cache-directory)
    (log:info "~@<Creating non-existent cache directory ~S.~@:>"
              cache-directory)
    (ensure-directories-exist cache-directory)))

(defun cache-restore (cache-directory key &key age-limit)
  (with-simple-restart (continue "~@<Do not use cache results.~@:>")
    (let ((file (merge-pathnames key cache-directory)))
      (log:info "~@<Maybe restoring analysis results in ~A~@:>" file)
      (when (probe-file file)
        (log:info "~@<Restoring analysis results in ~A~@:>" file)
        (let+ (((version timestamp data) (let ((entry (cl-store:restore file)))
                                           (typecase entry
                                             ((cons string (cons integer (cons t null)))
                                              entry)
                                             ((cons string t)
                                              (list (car entry) nil (cdr entry))))))
               (age))
          (cond ((not (string= version *cache-version*))
                 (log:warn "~@<Stored results have been produced by ~
                            version ~A while this is version ~A.~@:>"
                           version *cache-version*)
                 nil)
                ((and age-limit
                      (or (not timestamp)
                          (> (setf age (- (get-universal-time) timestamp))
                             age-limit)))
                 (log:info "~@<Stored analysis results have timestamp ~
                            ~D (~:D second~:P old) which is older ~
                            than ~:D second~:P; not using~@:>"
                           timestamp age age-limit)
                 nil)
                (t
                 data)))))))

(defun cache-store (cache-directory key results)
  (with-simple-restart (continue "~@<Do not cache results.~@:>")
    (unless (probe-file cache-directory)
      (log:info "~@<Creating non-existent cache directory ~S~@:>"
                cache-directory)
      (ensure-directories-exist cache-directory))
    (let ((file (merge-pathnames key cache-directory)))
      (log:info "~@<Storing analysis results in ~A~@:>" file)
      (let ((entry (list *cache-version* (get-universal-time) results)))
        (cl-store:store entry file)))))

(defun cache-or-compute (cache-directory key thunk &key age-limit)
  (let ((cachable? (and cache-directory key)))
    (or (when cachable?
          (cache-restore cache-directory key :age-limit age-limit))
        (let ((results (funcall thunk)))
          (when cachable?
            (cache-store cache-directory key results))
          results))))
