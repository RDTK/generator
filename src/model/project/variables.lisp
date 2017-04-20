;;;; variables.lisp --- Variables used by the project module.
;;;;
;;;; Copyright (C) 2013, 2014, 2015, 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.model.project)

(defmacro define-repository (name)
  "TODO(jmoringe): document"
  (let ((variable-name (format-symbol *package* "*~A~@(~P~)*" name 2))
        (lock-name     (format-symbol *package* "*~A~@(~P~)-LOCK*" name 2))
        (find-name     (format-symbol *package* "FIND-~A" name)))
   `(progn
      (defvar ,variable-name (make-hash-table :test #'equal)
        "TODO(jmoringe): document")

      (defvar ,lock-name (bt:make-lock)
        "TODO")

      (defun ,find-name (name &key (if-does-not-exist #'error))
        "TODO(jmoringe): document"
        (or (bt:with-lock-held (,lock-name)
              (gethash name ,variable-name))
            (etypecase if-does-not-exist
              (null
               if-does-not-exist)
              (function
               (funcall
                if-does-not-exist
                (make-condition
                 'simple-error
                 :format-control   ,(format nil "~~@<No such ~(~A~): ~~S.~~@:>"
                                            name)
                 :format-arguments (list name)))))))

      (defun (setf ,find-name) (new-value name &key if-does-not-exist)
        "TODO(jmoringe): document"
        (declare (ignore if-does-not-exist))

        (bt:with-lock-held (,lock-name)
          (when-let ((existing (gethash name ,variable-name)))
            (error "~@<The name ~S is already associated with ~A ~A.~@:>"
                   name ',name existing))
          (setf (gethash name ,variable-name) new-value))))))

;;; Global template repository

(define-repository template)

;;; Global project repository with provider lookup

(define-repository project)

(define-repository provider)

(defun provider-version (spec)
  (third spec))

(defun push-provider (new-value name)
  (bt:with-lock-held (*providers-lock*)
    (push new-value (gethash name *providers* '()))))

(defun providers/alist ()
  (expand-providers
   (bt:with-lock-held (*providers-lock*)
     (hash-table-alist *providers*))))

(defun expand-providers (providers)
  "Expand PROVIDERS from the form

     ((VERSION1 . (PROVIDER11 PROVIDER12 …))
      (VERSION2 . (PROVIDER21 PROVIDER22 …))
      …)

   to the form

     ((VERSION1 . PROVIDER11) … (VERSION2 . PROVIDER21) …)

   ."
  (iter outer
        (for (version . providers1) in providers)
        (iter (for provider in providers1)
              (in outer (collect (cons version provider))))))

(defun find-provider/version (spec
                              &key
                              (if-does-not-exist #'error)
                              (providers         (providers/alist))
                              order)
  (log:trace "~@<Trying to find provider of ~S~@:>" spec)
  (let+ (((mechanism name &optional version) spec)
         ((&flet version-better (left right)
            (cond
              ((equal version left) ; LEFT is an exact match => LEFT is best
               t)
              ((equal version right) ; RIGHT is an exact match => LEFT is not better
               nil)
              ;; VERSION is a prefix of LEFT but not of RIGHT => LEFT is better
              ((and (starts-with-subseq version left :test #'equal)
                    (not (starts-with-subseq version right :test #'equal)))
               t)
              ;; VERSION is a prefix of RIGHT (and maybe a prefix of
              ;; LEFT) => LEFT is not better
              ((starts-with-subseq version right :test #'equal)
               nil)
              ;; LEFT is greater than RIGHT => LEFT is better
              ((version>= left right)))))
         ((&flet provider-better (left right)
            ;; If ORDER is supplied, use it. If not (or ORDER returns
            ;; nil), use VERSION-BETTER.
            (or (and order (funcall order left right))
                (and (not (and order (funcall order right left)))
                     (version-better (provider-version (car left))
                                     (provider-version (car right)))))))
         ;; Find providers in PROVIDERS which can provide the required
         ;; MECHANISM, NAME and VERSION of SPEC.
         (candidates (remove-if-not (lambda+ ((mechanism1 name1 &optional version1))
                                      (and (eq              mechanism1 mechanism)
                                           (string=         name1      name)
                                           (version-matches version    version1)))
                                    providers
                                    :key #'car))
         ;; Sort CANDIDATES according to PROVIDER-BETTER (which may
         ;; use ORDER).
         (candidates (stable-sort candidates #'provider-better)))
    ;; Take the best candidate or act according to IF-DOES-NOT-EXIST.
    (or (cdr (first candidates))
        (error-behavior-restart-case
            (if-does-not-exist
             (jenkins.analysis:unfulfilled-project-dependency-error
              :dependency spec
              :candidates candidates)
             :allow-other-values? t)))))
