;;;; bcrypt.lisp --- An implementation of the bcrypt key derivation.
;;;;
;;;; Copyright (C) 2014-2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:build-generator.bcrypt)

;;; Constants

(define-constant +bcrypt-version+ "2a"
  :test #'string=)

(define-constant +cipher-text+
    (map 'octet-vector #'char-code "OrpheanBeholderScryDoubt")
  :test #'equalp)

(defconstant +salt-length+ 16)

(defconstant +salt-default-rounds+ 10)

;;;

(defun enhanced-mix-data (data offset)
  (let ((state 0))
    (declare (type (unsigned-byte 32) state))
    (dotimes (k 4)
      (setf state (logior (ironclad::mod32ash state 8)
                          (aref data (mod (+ offset k) (length data))))))
    state))

(macrolet ((s-box (s-boxes which index)
             `(aref ,s-boxes (+ (* 256 ,which) ,index))))

  (defun enhanced-initialize-blowfish-vectors (data key p-array s-boxes)
    (declare (type (simple-array (unsigned-byte 8) (*)) key)
             (type ironclad::blowfish-p-array p-array)
             (type ironclad::blowfish-s-boxes s-boxes))
    (ironclad::mix-p-array key p-array)
    (let ((state  (make-octet-vector 8))
          (offset 0))
      (declare (type (simple-octet-vector 8) state))

      (do ((i 0 (+ i 2)))
          ((= i (+ ironclad::+blowfish-n-rounds+ 2)))

        ;; This is the enhancement.
        (setf (ub32ref/be state 0) (logxor (ub32ref/be state 0) (enhanced-mix-data data offset))
              (ub32ref/be state 4) (logxor (ub32ref/be state 4) (enhanced-mix-data data (+ offset 4))))
        (incf offset 8)

        (ironclad::blowfish-encrypt-block* p-array s-boxes state 0 state 0)
        (setf (aref p-array i)      (ub32ref/be state 0)
              (aref p-array (1+ i)) (ub32ref/be state 4)))

      (dotimes (i 4)
        (do ((j 0 (+ j 2)))
            ((= j 256))

          ;; This is the enhancement.
          (setf (ub32ref/be state 0) (logxor (ub32ref/be state 0) (enhanced-mix-data data offset))
                (ub32ref/be state 4) (logxor (ub32ref/be state 4) (enhanced-mix-data data (+ offset 4))))
          (incf offset 8)

          (ironclad::blowfish-encrypt-block* p-array s-boxes state 0 state 0)
          (setf (s-box s-boxes i j)      (ub32ref/be state 0)
                (s-box s-boxes i (1+ j)) (ub32ref/be state 4)))))))

(defun blowfish (password salt rounds)
  (let* ((p-array (copy-seq ironclad::+p-array+))
         (s-boxes (make-array 1024 :element-type '(unsigned-byte 32)))
         (buffer  (copy-seq +cipher-text+)))
    ;; init_key
    (dotimes (i 256)
      (setf (aref s-boxes i)         (aref ironclad::+s-box-0+ i)
            (aref s-boxes (+ 256 i)) (aref ironclad::+s-box-1+ i)
            (aref s-boxes (+ 512 i)) (aref ironclad::+s-box-2+ i)
            (aref s-boxes (+ 768 i)) (aref ironclad::+s-box-3+ i)))

    (enhanced-initialize-blowfish-vectors
     salt password p-array s-boxes)

    (dotimes (i rounds)
      (ironclad::initialize-blowfish-vectors password p-array s-boxes)
      (ironclad::initialize-blowfish-vectors salt     p-array s-boxes))

    (dotimes (i 64)
      (dotimes (j (floor (length +cipher-text+) (* 4 2)))
        (let ((offset (* j 2)))
          (ironclad::blowfish-encrypt-block* p-array s-boxes buffer (* 4 offset) buffer (* 4 offset)))))

    buffer))

;;; Interface

(defun print-password-hash (rounds &rest material)
  (format nil "$~A$~2,'0D$~{~A~}"
          +bcrypt-version+ rounds (mapcar #'base64-encode material)))

(defun parse-password-hash (string)
  (ppcre:register-groups-bind (                 version
                               (#'parse-integer rounds)
                               (#'base64-decode material))
      ("^\\$([^$]+)\\$([0-9]+)\\$(.*)$" string)
    (values version rounds material)))

(defun make-salt (&optional (rounds +salt-default-rounds+))
  (let ((material (map-into (make-octet-vector +salt-length+)
                            (curry #'random 256))))
    (print-password-hash rounds material)))

(defun hash-password (password &key (salt (make-salt)))
  (let+ (((&values version rounds salt) (parse-password-hash salt))
         (password (concatenate 'octet-vector
                                (sb-ext:string-to-octets password)
                                #(0)))
         (result   (switch (version :test #'string=)
                     (+bcrypt-version+ (blowfish password salt (ash 1 rounds)))
                     (t                (error "~@<Only version ~A of bcrypt is ~
                                               supported.~@:>"
                                              +bcrypt-version+)))))
    (print-password-hash rounds salt (subseq result 0 (1- (length result))))))
