;;;; base64.lisp --- Bcrypt-specific variant of base64 encoding.
;;;;
;;;; Copyright (C) 2014-2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.project.bcrypt)

(alexandria:define-constant +base64-encoding+
    #(#\. #\/ #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N #\O
      #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z #\a #\b #\c #\d #\e #\f
      #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w
      #\x #\y #\z #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
  :test #'equalp)

(alexandria:define-constant +base64-decoding+
    #(-1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1
      -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1
      -1 -1  0  1 54 55 56 57 58 59 60 61 62 63 -1 -1 -1 -1 -1 -1 -1  2
       3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24
      25 26 27 -1 -1 -1 -1 -1 -1 28 29 30 31 32 33 34 35 36 37 38 39 40
      41 42 43 44 45 46 47 48 49 50 51 52 53 -1 -1 -1 -1 -1)
  :test #'equalp)

(defun base64-encode (data &key (start 0) (end (length data)) )
  (let+ ((result (make-array 0 :element-type 'character :fill-pointer 0))
         ((&flet output (x)
            (let ((index (ldb (byte 6 0) x)))
              (vector-push-extend (aref +base64-encoding+ index) result))))
         ((&flet maybe-done (offset &optional final)
            (when (>= offset end)
              (when final (output final))
              (throw 'done nil))))
         ((&flet one-block (offset)
            (maybe-done offset)
            (let ((c1 (aref data offset))
                  (c2))
              (output (ldb (byte 6 2) c1))
              (setf c1 (ash (ldb (byte 2 0) c1) 4))

              (maybe-done (+ offset 1) c1)
              (setf c2 (aref data (+ offset 1)))
              (setf c1 (logior c1 (ldb (byte 4 4) c2)))
              (output c1)
              (setf c1 (ash (ldb (byte 4 0) c2) 2))

              (maybe-done (+ offset 2) c1)
              (setf c2 (aref data (+ offset 2)))
              (setf c1 (logior c1 (ldb (byte 2 6) c2)))
              (output c1)
              (output c2)))))
    (catch 'done
      (loop :for offset :from start :by 3 :do (one-block offset)))
    result))

(defun base64-decode (string)
  (let+ ((length (length string))
         (result (make-array 0 :element-type 'nibbles:octet :fill-pointer 0))
         ((&flet input (offset)
            (let ((code (char-code (aref string offset))))
              (if (< 0 code (length +base64-decoding+))
                  (aref +base64-decoding+ code)
                  (throw 'done nil)))))
         ((&flet output (value)
            (vector-push-extend value result)))
         ((&flet maybe-done (offset)
            (when (>= offset length)
              (throw 'done nil))))
         ((&flet one-block (offset)
            (maybe-done offset)
            (let ((c1 (input offset))
                  (c2 (input (1+ offset)))
                  c3 c4)
              (output (dpb (ldb (byte 6 0) c1)
                           (byte 6 2)
                           (ldb (byte 2 4) c2)))

              (maybe-done (+ offset 2))
              (setf c3 (input (+ offset 2)))
              (output (dpb (ldb (byte 4 0) c2)
                           (byte 4 4)
                           (ldb (byte 4 2) c3)))

              (maybe-done (+ offset 3))
              (setf c4 (input (+ offset 3)))
              (output (dpb (ldb (byte 2 0) c3)
                           (byte 2 6)
                           c4))))))
    (catch 'done
      (loop :for offset :by 4 :do (one-block offset)))
    (coerce result 'nibbles:simple-octet-vector)))
