;;; md5.el -- MD5 Message Digest Algorithm
;;; Gareth Rees <gdr11@cl.cam.ac.uk>

;; LCD Archive Entry:
;; md5|Gareth Rees|gdr11@cl.cam.ac.uk|
;; MD5 cryptographic message digest algorithm|
;; 13-Nov-95|1.0|~/misc/md5.el.Z|

;;; Details: ------------------------------------------------------------------

;; This is a direct translation into Emacs LISP of the reference C
;; implementation of the MD5 Message-Digest Algorithm written by RSA
;; Data Security, Inc.
;; 
;; The algorithm takes a message (that is, a string of bytes) and
;; computes a 16-byte checksum or "digest" for the message.  This digest
;; is supposed to be cryptographically strong in the sense that if you
;; are given a 16-byte digest D, then there is no easier way to
;; construct a message whose digest is D than to exhaustively search the
;; space of messages.  However, the robustness of the algorithm has not
;; been proven, and a similar algorithm (MD4) was shown to be unsound,
;; so treat with caution!
;; 
;; The C algorithm uses 32-bit integers; because GNU Emacs
;; implementations provide 28-bit integers (with 24-bit integers on
;; versions prior to 19.29), the code represents a 32-bit integer as the
;; cons of two 16-bit integers.  The most significant word is stored in
;; the car and the least significant in the cdr.  The algorithm requires
;; at least 17 bits of integer representation in order to represent the
;; carry from a 16-bit addition.

;;; Usage: --------------------------------------------------------------------

;; To compute the MD5 Message Digest for a message M (represented as a
;; string or as a vector of bytes), call
;; 
;;   (md5-encode M)
;; 
;; which returns the message digest as a vector of 16 bytes.  If you
;; need to supply the message in pieces M1, M2, ... Mn, then call
;; 
;;   (md5-init)
;;   (md5-update M1)
;;   (md5-update M2)
;;   ...
;;   (md5-update Mn)
;;   (md5-final)

;;; Copyright and licence: ----------------------------------------------------

;; Copyright (C) 1995 by Gareth Rees
;; Derived from the RSA Data Security, Inc. MD5 Message-Digest Algorithm
;; 
;; md5.el is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 2, or (at your option) any
;; later version.
;; 
;; md5.el is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;; 
;; The original copyright notice is given below, as required by the
;; licence for the original code.  This code is distributed under *both*
;; RSA's original licence and the GNU General Public Licence.  (There
;; should be no problems, as the former is more liberal than the
;; latter).

;;; Original copyright notice: ------------------------------------------------

;; Copyright (C) 1990, RSA Data Security, Inc. All rights reserved.
;;
;; License to copy and use this software is granted provided that it is
;; identified as the "RSA Data Security, Inc. MD5 Message- Digest
;; Algorithm" in all material mentioning or referencing this software or
;; this function.
;;
;; License is also granted to make and use derivative works provided
;; that such works are identified as "derived from the RSA Data
;; Security, Inc. MD5 Message-Digest Algorithm" in all material
;; mentioning or referencing the derived work.
;;
;; RSA Data Security, Inc. makes no representations concerning either
;; the merchantability of this software or the suitability of this
;; software for any particular purpose.  It is provided "as is" without
;; express or implied warranty of any kind.
;;
;; These notices must be retained in any copies of any part of this
;; documentation and/or software.

;;; Code: ---------------------------------------------------------------------

(defvar md5-program "md5"
  "*Program that reads a message on its standard input and writes an
MD5 digest on its output.")

(defvar md5-maximum-internal-length 4096
  "*The maximum size of a piece of data that should use the MD5 routines
written in lisp.  If a message exceeds this, it will be run through an
external filter for processing.  Also see the `md5-program' variable.
This variable has no effect if you call the md5-init|update|final
functions - only used by the `md5' function's simpler interface.")

(defvar md5-bits (make-vector 4 0)
  "Number of bits handled, modulo 2^64.
Represented as four 16-bit numbers, least significant first.")
(defvar md5-buffer (make-vector 4 '(0 . 0))
  "Scratch buffer (four 32-bit integers).")
(defvar md5-input (make-vector 64 0)
  "Input buffer (64 bytes).")

(defun md5-unhex (x)
  (if (> x ?9)
      (if (>= x ?a)
	  (+ 10 (- x ?a))
	(+ 10 (- x ?A)))
    (- x ?0)))

(defun md5-encode (message)
  "Encodes MESSAGE using the MD5 message digest algorithm.
MESSAGE must be a string or an array of bytes.
Returns a vector of 16 bytes containing the message digest."
  (if (<= (length message) md5-maximum-internal-length)
      (progn
	(md5-init)
	(md5-update message)
	(md5-final))
    (save-excursion
      (set-buffer (get-buffer-create " *md5-work*"))
      (erase-buffer)
      (insert message)
      (call-process-region (point-min) (point-max)
			   (or shell-file-name "/bin/sh")
			   t (current-buffer) nil
			   "-c" md5-program)
      ;; MD5 digest is 32 chars long
      ;; mddriver adds a newline to make neaten output for tty
      ;; viewing, make sure we leave it behind.
      (let ((data (buffer-substring (point-min) (+ (point-min) 32)))
	    (vec (make-vector 16 0))
	    (ctr 0))
	(while (< ctr 16)
	  (aset vec ctr (+ (* 16 (md5-unhex (aref data (* ctr 2))))
			   (md5-unhex (aref data (1+ (* ctr 2))))))
	  (setq ctr (1+ ctr)))))))

(defsubst md5-add (x y)
  "Return 32-bit sum of 32-bit integers X and Y."
  (let ((m (+ (car x) (car y)))
        (l (+ (cdr x) (cdr y))))
    (cons (logand 65535 (+ m (lsh l -16))) (logand l 65535))))

;; FF, GG, HH and II are basic MD5 functions, providing transformations
;; for rounds 1, 2, 3 and 4 respectively.  Each function follows this
;; pattern of computation (where ROTATE(x,y) means rotate 32-bit value x
;; by y bits to the left):
;; 
;;   FF(a,b,c,d,x,s,ac) = ROTATE(a + F(b,c,d) + x + ac,s) + b
;; 
;; so we use the macro `md5-make-step' to construct each one.  The
;; helper functions F, G, H and I operate on 16-bit numbers; the full
;; operation splits its inputs, operates on the halves separately and
;; then puts the results together.

(defsubst md5-F (x y z) (logior (logand x y) (logand (lognot x) z)))
(defsubst md5-G (x y z) (logior (logand x z) (logand y (lognot z))))
(defsubst md5-H (x y z) (logxor x y z))
(defsubst md5-I (x y z) (logxor y (logior x (logand 65535 (lognot z)))))

(defmacro md5-make-step (name func)
  (`
   (defun (, name) (a b c d x s ac)
     (let*
         ((m1 (+ (car a) ((, func) (car b) (car c) (car d)) (car x) (car ac)))
          (l1 (+ (cdr a) ((, func) (cdr b) (cdr c) (cdr d)) (cdr x) (cdr ac)))
          (m2 (logand 65535 (+ m1 (lsh l1 -16))))
          (l2 (logand 65535 l1))
          (m3 (logand 65535 (if (> s 15)
                                (+ (lsh m2 (- s 32)) (lsh l2 (- s 16)))
                              (+ (lsh m2 s) (lsh l2 (- s 16))))))
          (l3 (logand 65535 (if (> s 15)
                                (+ (lsh l2 (- s 32)) (lsh m2 (- s 16)))
                              (+ (lsh l2 s) (lsh m2 (- s 16)))))))
       (md5-add (cons m3 l3) b)))))

(md5-make-step md5-FF md5-F)
(md5-make-step md5-GG md5-G)
(md5-make-step md5-HH md5-H)
(md5-make-step md5-II md5-I)

(defun md5-init ()
  "Initialise the state of the message-digest routines."
  (aset md5-bits 0 0)
  (aset md5-bits 1 0)
  (aset md5-bits 2 0)
  (aset md5-bits 3 0)
  (aset md5-buffer 0 '(26437 .  8961))
  (aset md5-buffer 1 '(61389 . 43913))
  (aset md5-buffer 2 '(39098 . 56574))
  (aset md5-buffer 3 '( 4146 . 21622)))

(defun md5-update (string)
  "Update the current MD5 state with STRING (an array of bytes)."
  (let ((len (length string))
        (i 0)
        (j 0))
    (while (< i len)
      ;; Compute number of bytes modulo 64
      (setq j (% (/ (aref md5-bits 0) 8) 64))

      ;; Store this byte (truncating to 8 bits to be sure)
      (aset md5-input j (logand 255 (aref string i)))

      ;; Update number of bits by 8 (modulo 2^64)
      (let ((c 8) (k 0))
        (while (and (> c 0) (< k 4))
          (let ((b (aref md5-bits k)))
            (aset md5-bits k (logand 65535 (+ b c)))
            (setq c (if (> b (- 65535 c)) 1 0)
                  k (1+ k)))))

      ;; Increment number of bytes processed
      (setq i (1+ i))

      ;; When 64 bytes accumulated, pack them into sixteen 32-bit
      ;; integers in the array `in' and then tranform them.
      (if (= j 63)
          (let ((in (make-vector 16 (cons 0 0)))
                (k 0)
                (kk 0))
            (while (< k 16)
              (aset in k (md5-pack md5-input kk))
              (setq k (+ k 1) kk (+ kk 4)))
            (md5-transform in))))))

(defun md5-pack (array i)
  "Pack the four bytes at ARRAY reference I to I+3 into a 32-bit integer."
  (cons (+ (lsh (aref array (+ i 3)) 8) (aref array (+ i 2)))
        (+ (lsh (aref array (+ i 1)) 8) (aref array (+ i 0)))))

(defun md5-byte (array n b)
  "Unpack byte B (0 to 3) from Nth member of ARRAY of 32-bit integers."
  (let ((e (aref array n)))
    (cond ((eq b 0) (logand 255 (cdr e)))
          ((eq b 1) (lsh (cdr e) -8))
          ((eq b 2) (logand 255 (car e)))
          ((eq b 3) (lsh (car e) -8)))))

(defun md5-final ()
  (let ((in (make-vector 16 (cons 0 0)))
        (j 0)
        (digest (make-vector 16 0))
        (padding))

    ;; Save the number of bits in the message
    (aset in 14 (cons (aref md5-bits 1) (aref md5-bits 0)))
    (aset in 15 (cons (aref md5-bits 3) (aref md5-bits 2)))

    ;; Compute number of bytes modulo 64
    (setq j (% (/ (aref md5-bits 0) 8) 64))

    ;; Pad out computation to 56 bytes modulo 64
    (setq padding (make-vector (if (< j 56) (- 56 j) (- 120 j)) 0))
    (aset padding 0 128)
    (md5-update padding)

    ;; Append length in bits and transform
    (let ((k 0) (kk 0))
      (while (< k 14)
        (aset in k (md5-pack md5-input kk))
        (setq k (+ k 1) kk (+ kk 4))))
    (md5-transform in)

    ;; Store the results in the digest
    (let ((k 0) (kk 0))
      (while (< k 4)
        (aset digest (+ kk 0) (md5-byte md5-buffer k 0))
        (aset digest (+ kk 1) (md5-byte md5-buffer k 1))
        (aset digest (+ kk 2) (md5-byte md5-buffer k 2))
        (aset digest (+ kk 3) (md5-byte md5-buffer k 3))
        (setq k (+ k 1) kk (+ kk 4))))

    ;; Return digest
    digest))

;; It says in the RSA source, "Note that if the Mysterious Constants are
;; arranged backwards in little-endian order and decrypted with the DES
;; they produce OCCULT MESSAGES!"  Security through obscurity?

(defun md5-transform (in)
  "Basic MD5 step. Transform md5-buffer based on array IN."
  (let ((a (aref md5-buffer 0))
        (b (aref md5-buffer 1))
        (c (aref md5-buffer 2))
        (d (aref md5-buffer 3)))
    (setq
     a (md5-FF a b c d (aref in  0)  7 '(55146 . 42104))
     d (md5-FF d a b c (aref in  1) 12 '(59591 . 46934))
     c (md5-FF c d a b (aref in  2) 17 '( 9248 . 28891))
     b (md5-FF b c d a (aref in  3) 22 '(49597 . 52974))
     a (md5-FF a b c d (aref in  4)  7 '(62844 .  4015))
     d (md5-FF d a b c (aref in  5) 12 '(18311 . 50730))
     c (md5-FF c d a b (aref in  6) 17 '(43056 . 17939))
     b (md5-FF b c d a (aref in  7) 22 '(64838 . 38145))
     a (md5-FF a b c d (aref in  8)  7 '(27008 . 39128))
     d (md5-FF d a b c (aref in  9) 12 '(35652 . 63407))
     c (md5-FF c d a b (aref in 10) 17 '(65535 . 23473))
     b (md5-FF b c d a (aref in 11) 22 '(35164 . 55230))
     a (md5-FF a b c d (aref in 12)  7 '(27536 .  4386))
     d (md5-FF d a b c (aref in 13) 12 '(64920 . 29075))
     c (md5-FF c d a b (aref in 14) 17 '(42617 . 17294))
     b (md5-FF b c d a (aref in 15) 22 '(18868 .  2081))
     a (md5-GG a b c d (aref in  1)  5 '(63006 .  9570))
     d (md5-GG d a b c (aref in  6)  9 '(49216 . 45888))
     c (md5-GG c d a b (aref in 11) 14 '( 9822 . 23121))
     b (md5-GG b c d a (aref in  0) 20 '(59830 . 51114))
     a (md5-GG a b c d (aref in  5)  5 '(54831 .  4189))
     d (md5-GG d a b c (aref in 10)  9 '(  580 .  5203))
     c (md5-GG c d a b (aref in 15) 14 '(55457 . 59009))
     b (md5-GG b c d a (aref in  4) 20 '(59347 . 64456))
     a (md5-GG a b c d (aref in  9)  5 '( 8673 . 52710))
     d (md5-GG d a b c (aref in 14)  9 '(49975 .  2006))
     c (md5-GG c d a b (aref in  3) 14 '(62677 .  3463))
     b (md5-GG b c d a (aref in  8) 20 '(17754 .  5357))
     a (md5-GG a b c d (aref in 13)  5 '(43491 . 59653))
     d (md5-GG d a b c (aref in  2)  9 '(64751 . 41976))
     c (md5-GG c d a b (aref in  7) 14 '(26479 .   729))
     b (md5-GG b c d a (aref in 12) 20 '(36138 . 19594))
     a (md5-HH a b c d (aref in  5)  4 '(65530 . 14658))
     d (md5-HH d a b c (aref in  8) 11 '(34673 . 63105))
     c (md5-HH c d a b (aref in 11) 16 '(28061 . 24866))
     b (md5-HH b c d a (aref in 14) 23 '(64997 . 14348))
     a (md5-HH a b c d (aref in  1)  4 '(42174 . 59972))
     d (md5-HH d a b c (aref in  4) 11 '(19422 . 53161))
     c (md5-HH c d a b (aref in  7) 16 '(63163 . 19296))
     b (md5-HH b c d a (aref in 10) 23 '(48831 . 48240))
     a (md5-HH a b c d (aref in 13)  4 '(10395 . 32454))
     d (md5-HH d a b c (aref in  0) 11 '(60065 . 10234))
     c (md5-HH c d a b (aref in  3) 16 '(54511 . 12421))
     b (md5-HH b c d a (aref in  6) 23 '( 1160 .  7429))
     a (md5-HH a b c d (aref in  9)  4 '(55764 . 53305))
     d (md5-HH d a b c (aref in 12) 11 '(59099 . 39397))
     c (md5-HH c d a b (aref in 15) 16 '( 8098 . 31992))
     b (md5-HH b c d a (aref in  2) 23 '(50348 . 22117))
     a (md5-II a b c d (aref in  0)  6 '(62505 .  8772))
     d (md5-II d a b c (aref in  7) 10 '(17194 . 65431))
     c (md5-II c d a b (aref in 14) 15 '(43924 .  9127))
     b (md5-II b c d a (aref in  5) 21 '(64659 . 41017))
     a (md5-II a b c d (aref in 12)  6 '(25947 . 22979))
     d (md5-II d a b c (aref in  3) 10 '(36620 . 52370))
     c (md5-II c d a b (aref in 10) 15 '(65519 . 62589))
     b (md5-II b c d a (aref in  1) 21 '(34180 . 24017))
     a (md5-II a b c d (aref in  8)  6 '(28584 . 32335))
     d (md5-II d a b c (aref in 15) 10 '(65068 . 59104))
     c (md5-II c d a b (aref in  6) 15 '(41729 . 17172))
     b (md5-II b c d a (aref in 13) 21 '(19976 .  4513))
     a (md5-II a b c d (aref in  4)  6 '(63315 . 32386))
     d (md5-II d a b c (aref in 11) 10 '(48442 . 62005))
     c (md5-II c d a b (aref in  2) 15 '(10967 . 53947))
     b (md5-II b c d a (aref in  9) 21 '(60294 . 54161)))

     (aset md5-buffer 0 (md5-add (aref md5-buffer 0) a))
     (aset md5-buffer 1 (md5-add (aref md5-buffer 1) b))
     (aset md5-buffer 2 (md5-add (aref md5-buffer 2) c))
     (aset md5-buffer 3 (md5-add (aref md5-buffer 3) d))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Here begins the merger with the XEmacs API and the md5.el from the URL
;;; package.  Courtesy wmperry@spry.com
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun md5 (object &optional start end)
  "Return the MD5 (a secure message digest algorithm) of an object.
OBJECT is either a string or a buffer.
Optional arguments START and END denote buffer positions for computing the
hash of a portion of OBJECT."
 (let ((buffer nil))
    (unwind-protect
	(save-excursion
	  (setq buffer (generate-new-buffer " *md5-work*"))
	  (set-buffer buffer)
	  (cond
	   ((bufferp object)
	    (insert-buffer-substring object start end))
	   ((stringp object)
	    (insert (if (or start end)
			(substring object start end)
		      object)))
	   (t nil))
	  (prog1
	      (if (<= (point-max) md5-maximum-internal-length)
		  (mapconcat
		   (function (lambda (node) (format "%02x" node)))
		   (md5-encode (buffer-string))
		   "")
		(call-process-region (point-min) (point-max)
				     (or shell-file-name "/bin/sh")
				     t buffer nil
				     "-c" md5-program)
		;; MD5 digest is 32 chars long
		;; mddriver adds a newline to make neaten output for tty
		;; viewing, make sure we leave it behind.
		(buffer-substring (point-min) (+ (point-min) 32)))
	    (kill-buffer buffer)))
      (and buffer (kill-buffer buffer) nil))))

(provide 'md5)

;;; md5.el ends here ----------------------------------------------------------
