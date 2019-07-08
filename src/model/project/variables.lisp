;;;; variables.lisp --- Variables used by the project module.
;;;;
;;;; Copyright (C) 2013-2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:build-generator.model.project)

(defmacro define-repository (name)
  (let ((variable-name (format-symbol *package* "*~A~@(~P~)*" name 2))
        (lock-name     (format-symbol *package* "*~A~@(~P~)-LOCK*" name 2))
        (find-name     (format-symbol *package* "FIND-~A" name)))
   `(progn
      (defvar ,variable-name (make-hash-table :test #'equal))

      (defvar ,lock-name (bt:make-lock))

      (defun ,find-name (name &key (if-does-not-exist #'error))
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

(defun find-provider/version (spec providers &key (if-does-not-exist #'error))
  (log:trace "~@<Trying to find provider of ~S~@:>" spec)
  (let+ (((&whole required &ign &ign &optional version) spec)
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
            (version-better (third (car left)) (third (car right)))))
         ;; Find providers in PROVIDERS which can provide the required
         ;; NATURE, TARGET and VERSION of SPEC.
         (candidates (analysis:lookup-providers required providers))
         (candidates (remove-if-not
                      (curry #'analysis:dependency-matches? required)
                      candidates :key #'car))
         ;; Sort CANDIDATES according to PROVIDER-BETTER (which may
         ;; use ORDER).
         (candidates (stable-sort candidates #'provider-better)))
    ;; Take the best candidate or act according to IF-DOES-NOT-EXIST.
    (or (cdr (first candidates))
        (error-behavior-restart-case
            (if-does-not-exist
             (analysis:unfulfilled-project-dependency-error
              :dependency spec
              :candidates candidates)
             :allow-other-values? t)))))

;;; People

(defvar *persons* '())

(defvar *persons-lock* (bt:make-lock "global person list"))

(defun all-persons ()
  (bt:with-lock-held (*persons-lock*)
    (copy-list *persons*)))

(defun ensure-persons! (people)
  (bt:with-lock-held (*persons-lock*)
    (let+ (((&values all-people &ign new-people)
            (rosetta-project.model.resource:merge-persons! people *persons*)))
      (setf *persons* all-people)
      new-people)))
