;;; sha1.el --- SHA1 Secure Hash Algorithm in Emacs-Lisp

;; Copyright (C) 1999, 2001, 2002, 2003, 2004,
;;   2005, 2006, 2007, 2008 Free Software Foundation, Inc.

;; Author: Shuhei KOBAYASHI <shuhei@aqua.ocn.ne.jp>
;; Keywords: SHA1, FIPS 180-1

;; This file is part of FLIM (Faithful Library about Internet Message).

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This program is implemented from the definition of SHA-1 in FIPS PUB
;; 180-1 (Federal Information Processing Standards Publication 180-1),
;; "Announcing the Standard for SECURE HASH STANDARD".
;; <URL:http://www.itl.nist.gov/div897/pubs/fip180-1.htm>
;; (EXCEPTION; two optimizations taken from GnuPG/cipher/sha1.c)
;;
;; Test cases from FIPS PUB 180-1.
;;
;; (sha1 "abc")
;; => a9993e364706816aba3e25717850c26c9cd0d89d
;;
;; (sha1 "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq")
;; => 84983e441c3bd26ebaae4aa1f95129e5e54670f1
;;
;; (sha1 (make-string 1000000 ?a))
;; => 34aa973cd4c4daa4f61eeb2bdbad27316534016f
;;
;; BUGS:
;;  * It is assumed that length of input string is less than 2^29 bytes.
;;  * It is caller's responsibility to make string (or region) unibyte.
;;
;; TODO:
;;  * Rewrite from scratch!
;;    This version is much faster than Keiichi Suzuki's another sha1.el,
;;    but it is too dirty.

;;; Code:

(require 'hex-util)

;;;
;;; external SHA1 function.
;;;

(defgroup sha1 nil
  "Elisp interface for SHA1 hash computation."
  :version "22.1"
  :group 'extensions)

(defcustom sha1-maximum-internal-length 500
  "*Maximum length of message to use Lisp version of SHA1 function.
If message is longer than this, `sha1-program' is used instead.

If this variable is set to 0, use external program only.
If this variable is set to nil, use internal function only."
  :type 'integer
  :group 'sha1)

(defcustom sha1-program '("sha1sum")
  "*Name of program to compute SHA1.
It must be a string \(program name\) or list of strings \(name and its args\)."
  :type '(repeat string)
  :group 'sha1)

(defcustom sha1-use-external (condition-case ()
				 (executable-find (car sha1-program))
			       (error))
  "*Use external SHA1 program.
If this variable is set to nil, use internal function only."
  :type 'boolean
  :group 'sha1)

(defun sha1-string-external (string &optional binary)
  (let (prog args digest default-enable-multibyte-characters)
    (if (consp sha1-program)
	(setq prog (car sha1-program)
	      args (cdr sha1-program))
      (setq prog sha1-program
	    args nil))
    (with-temp-buffer
      (insert string)
      (apply (function call-process-region)
	     (point-min)(point-max)
	     prog t t nil args)
      ;; SHA1 is 40 bytes long in hexadecimal form.
      (setq digest (buffer-substring (point-min)(+ (point-min) 40))))
    (if binary
	(decode-hex-string digest)
      digest)))

(defun sha1-region-external (beg end &optional binary)
  (sha1-string-external (buffer-substring-no-properties beg end) binary))

;;;
;;; internal SHA1 function.
;;;

(eval-when-compile
  ;; optional second arg of string-to-number is new in v20.
  (defconst sha1-K0-high 23170)		; (string-to-number "5A82" 16)
  (defconst sha1-K0-low  31129)		; (string-to-number "7999" 16)
  (defconst sha1-K1-high 28377)		; (string-to-number "6ED9" 16)
  (defconst sha1-K1-low  60321)		; (string-to-number "EBA1" 16)
  (defconst sha1-K2-high 36635)		; (string-to-number "8F1B" 16)
  (defconst sha1-K2-low  48348)		; (string-to-number "BCDC" 16)
  (defconst sha1-K3-high 51810)		; (string-to-number "CA62" 16)
  (defconst sha1-K3-low  49622)		; (string-to-number "C1D6" 16)

  ;; original definition of sha1-F0.
  ;; (defmacro sha1-F0 (B C D)
  ;;   (` (logior (logand (, B) (, C))
  ;; 	     (logand (lognot (, B)) (, D)))))
  ;; a little optimization from GnuPG/cipher/sha1.c.
  (defmacro sha1-F0 (B C D)
    `(logxor ,D (logand ,B (logxor ,C ,D))))
  (defmacro sha1-F1 (B C D)
    `(logxor ,B ,C ,D))
  ;; original definition of sha1-F2.
  ;; (defmacro sha1-F2 (B C D)
  ;;   (` (logior (logand (, B) (, C))
  ;; 	     (logand (, B) (, D))
  ;; 	     (logand (, C) (, D)))))
  ;; a little optimization from GnuPG/cipher/sha1.c.
  (defmacro sha1-F2 (B C D)
    `(logior (logand ,B ,C)
	       (logand ,D (logior ,B ,C))))
  (defmacro sha1-F3 (B C D)
    `(logxor ,B ,C ,D))

  (defmacro sha1-S1  (W-high W-low)
    `(let ((W-high ,W-high)
           (W-low  ,W-low))
	 (setq S1W-high (+ (% (* W-high 2) 65536)
			   (/ W-low ,(/ 65536 2))))
	 (setq S1W-low (+ (/ W-high ,(/ 65536 2))
			  (% (* W-low 2) 65536)))))
  (defmacro sha1-S5  (A-high A-low)
    `(progn
       (setq S5A-high (+ (% (* ,A-high 32) 65536)
                         (/ ,A-low ,(/ 65536 32))))
       (setq S5A-low  (+ (/ ,A-high ,(/ 65536 32))
                         (% (* ,A-low 32) 65536)))))
  (defmacro sha1-S30 (B-high B-low)
    `(progn
       (setq S30B-high (+ (/ ,B-high 4)
                          (* (% ,B-low 4) ,(/ 65536 4))))
       (setq S30B-low  (+ (/ ,B-low 4)
                          (* (% ,B-high 4) ,(/ 65536 4))))))

  (defmacro sha1-OP (round)
    `(progn
       (sha1-S5 sha1-A-high sha1-A-low)
       (sha1-S30 sha1-B-high sha1-B-low)
       (setq sha1-A-low (+ (,(intern (format "sha1-F%d" round))
                            sha1-B-low sha1-C-low sha1-D-low)
                           sha1-E-low
                           ,(symbol-value
                             (intern (format "sha1-K%d-low" round)))
                           (aref block-low idx)
                           (progn
                             (setq sha1-E-low sha1-D-low)
                             (setq sha1-D-low sha1-C-low)
                             (setq sha1-C-low S30B-low)
                             (setq sha1-B-low sha1-A-low)
                             S5A-low)))
       (setq carry (/ sha1-A-low 65536))
       (setq sha1-A-low (% sha1-A-low 65536))
       (setq sha1-A-high (% (+ (,(intern (format "sha1-F%d" round))
                                sha1-B-high sha1-C-high sha1-D-high)
                               sha1-E-high
                               ,(symbol-value
                                 (intern (format "sha1-K%d-high" round)))
                               (aref block-high idx)
                               (progn
                                 (setq sha1-E-high sha1-D-high)
                                 (setq sha1-D-high sha1-C-high)
                                 (setq sha1-C-high S30B-high)
                                 (setq sha1-B-high sha1-A-high)
                                 S5A-high)
                               carry)
                            65536))))

  (defmacro sha1-add-to-H (H X)
    `(progn
       (setq ,(intern (format "sha1-%s-low" H))
             (+ ,(intern (format "sha1-%s-low" H))
                ,(intern (format "sha1-%s-low" X))))
       (setq carry (/ ,(intern (format "sha1-%s-low" H)) 65536))
       (setq ,(intern (format "sha1-%s-low" H))
             (% ,(intern (format "sha1-%s-low" H)) 65536))
       (setq ,(intern (format "sha1-%s-high" H))
             (% (+ ,(intern (format "sha1-%s-high" H))
                   ,(intern (format "sha1-%s-high" X))
                   carry)
                65536))))
  )

;;; buffers (H0 H1 H2 H3 H4).
(defvar sha1-H0-high)
(defvar sha1-H0-low)
(defvar sha1-H1-high)
(defvar sha1-H1-low)
(defvar sha1-H2-high)
(defvar sha1-H2-low)
(defvar sha1-H3-high)
(defvar sha1-H3-low)
(defvar sha1-H4-high)
(defvar sha1-H4-low)

(defun sha1-block (block-high block-low)
  (let (;; step (c) --- initialize buffers (A B C D E).
	(sha1-A-high sha1-H0-high) (sha1-A-low sha1-H0-low)
	(sha1-B-high sha1-H1-high) (sha1-B-low sha1-H1-low)
	(sha1-C-high sha1-H2-high) (sha1-C-low sha1-H2-low)
	(sha1-D-high sha1-H3-high) (sha1-D-low sha1-H3-low)
	(sha1-E-high sha1-H4-high) (sha1-E-low sha1-H4-low)
	(idx 16))
    ;; step (b).
    (let (;; temporary variables used in sha1-S1 macro.
	  S1W-high S1W-low)
      (while (< idx 80)
	(sha1-S1 (logxor (aref block-high (- idx 3))
			 (aref block-high (- idx 8))
			 (aref block-high (- idx 14))
			 (aref block-high (- idx 16)))
		 (logxor (aref block-low  (- idx 3))
			 (aref block-low  (- idx 8))
			 (aref block-low  (- idx 14))
			 (aref block-low  (- idx 16))))
	(aset block-high idx S1W-high)
	(aset block-low  idx S1W-low)
	(setq idx (1+ idx))))
    ;; step (d).
    (setq idx 0)
    (let (;; temporary variables used in sha1-OP macro.
	  S5A-high S5A-low S30B-high S30B-low carry)
      (while (< idx 20) (sha1-OP 0) (setq idx (1+ idx)))
      (while (< idx 40) (sha1-OP 1) (setq idx (1+ idx)))
      (while (< idx 60) (sha1-OP 2) (setq idx (1+ idx)))
      (while (< idx 80) (sha1-OP 3) (setq idx (1+ idx))))
    ;; step (e).
    (let (;; temporary variables used in sha1-add-to-H macro.
	  carry)
      (sha1-add-to-H H0 A)
      (sha1-add-to-H H1 B)
      (sha1-add-to-H H2 C)
      (sha1-add-to-H H3 D)
      (sha1-add-to-H H4 E))))

(defun sha1-binary (string)
  "Return the SHA1 of STRING in binary form."
  (let (;; prepare buffers for a block. byte-length of block is 64.
	;; input block is split into two vectors.
	;;
	;; input block: 00 01 02 03 04 05 06 07 08 09 0A 0B 0C 0D 0E 0F ...
	;; block-high:  +-0-+       +-1-+       +-2-+       +-3-+
	;; block-low:         +-0-+       +-1-+       +-2-+       +-3-+
	;;
	;; length of each vector is 80, and elements of each vector are
	;; 16bit integers.  elements 0x10-0x4F of each vector are
	;; assigned later in `sha1-block'.
	(block-high (eval-when-compile (make-vector 80 nil)))
	(block-low  (eval-when-compile (make-vector 80 nil))))
    (unwind-protect
	(let* (;; byte-length of input string.
	       (len (length string))
	       (lim (* (/ len 64) 64))
	       (rem (% len 4))
	       (idx 0)(pos 0))
	  ;; initialize buffers (H0 H1 H2 H3 H4).
	  (setq sha1-H0-high 26437	; (string-to-number "6745" 16)
		sha1-H0-low  8961	; (string-to-number "2301" 16)
		sha1-H1-high 61389	; (string-to-number "EFCD" 16)
		sha1-H1-low  43913	; (string-to-number "AB89" 16)
		sha1-H2-high 39098	; (string-to-number "98BA" 16)
		sha1-H2-low  56574	; (string-to-number "DCFE" 16)
		sha1-H3-high 4146	; (string-to-number "1032" 16)
		sha1-H3-low  21622	; (string-to-number "5476" 16)
		sha1-H4-high 50130	; (string-to-number "C3D2" 16)
		sha1-H4-low  57840)	; (string-to-number "E1F0" 16)
	  ;; loop for each 64 bytes block.
	  (while (< pos lim)
	    ;; step (a).
	    (setq idx 0)
	    (while (< idx 16)
	      (aset block-high idx (+ (* (aref string pos) 256)
				      (aref string (1+ pos))))
	      (setq pos (+ pos 2))
	      (aset block-low  idx (+ (* (aref string pos) 256)
				      (aref string (1+ pos))))
	      (setq pos (+ pos 2))
	      (setq idx (1+ idx)))
	    (sha1-block block-high block-low))
	  ;; last block.
	  (if (prog1
		  (< (- len lim) 56)
		(setq lim (- len rem))
		(setq idx 0)
		(while (< pos lim)
		  (aset block-high idx (+ (* (aref string pos) 256)
					  (aref string (1+ pos))))
		  (setq pos (+ pos 2))
		  (aset block-low  idx (+ (* (aref string pos) 256)
					  (aref string (1+ pos))))
		  (setq pos (+ pos 2))
		  (setq idx (1+ idx)))
		;; this is the last (at most) 32bit word.
		(cond
		 ((= rem 3)
		  (aset block-high idx (+ (* (aref string pos) 256)
					  (aref string (1+ pos))))
		  (setq pos (+ pos 2))
		  (aset block-low  idx (+ (* (aref string pos) 256)
					  128)))
		 ((= rem 2)
		  (aset block-high idx (+ (* (aref string pos) 256)
					  (aref string (1+ pos))))
		  (aset block-low  idx 32768))
		 ((= rem 1)
		  (aset block-high idx (+ (* (aref string pos) 256)
					  128))
		  (aset block-low  idx 0))
		 (t ;; (= rem 0)
		  (aset block-high idx 32768)
		  (aset block-low  idx 0)))
		(setq idx (1+ idx))
		(while (< idx 16)
		  (aset block-high idx 0)
		  (aset block-low  idx 0)
		  (setq idx (1+ idx))))
	      ;; last block has enough room to write the length of string.
	      (progn
		;; write bit length of string to last 4 bytes of the block.
		(aset block-low  15 (* (% len 8192) 8))
		(setq len (/ len 8192))
		(aset block-high 15 (% len 65536))
		;; XXX: It is not practical to compute SHA1 of
		;;      such a huge message on emacs.
		;; (setq len (/ len 65536))	; for 64bit emacs.
		;; (aset block-low  14 (% len 65536))
		;; (aset block-high 14 (/ len 65536))
		(sha1-block block-high block-low))
	    ;; need one more block.
	    (sha1-block block-high block-low)
	    (fillarray block-high 0)
	    (fillarray block-low  0)
	    ;; write bit length of string to last 4 bytes of the block.
	    (aset block-low  15 (* (% len 8192) 8))
	    (setq len (/ len 8192))
	    (aset block-high 15 (% len 65536))
	    ;; XXX: It is not practical to compute SHA1 of
	    ;;      such a huge message on emacs.
	    ;; (setq len (/ len 65536))		; for 64bit emacs.
	    ;; (aset block-low  14 (% len 65536))
	    ;; (aset block-high 14 (/ len 65536))
	    (sha1-block block-high block-low))
	  ;; make output string (in binary form).
	  (let ((result (make-string 20 0)))
	    (aset result  0 (/ sha1-H0-high 256))
	    (aset result  1 (% sha1-H0-high 256))
	    (aset result  2 (/ sha1-H0-low  256))
	    (aset result  3 (% sha1-H0-low  256))
	    (aset result  4 (/ sha1-H1-high 256))
	    (aset result  5 (% sha1-H1-high 256))
	    (aset result  6 (/ sha1-H1-low  256))
	    (aset result  7 (% sha1-H1-low  256))
	    (aset result  8 (/ sha1-H2-high 256))
	    (aset result  9 (% sha1-H2-high 256))
	    (aset result 10 (/ sha1-H2-low  256))
	    (aset result 11 (% sha1-H2-low  256))
	    (aset result 12 (/ sha1-H3-high 256))
	    (aset result 13 (% sha1-H3-high 256))
	    (aset result 14 (/ sha1-H3-low  256))
	    (aset result 15 (% sha1-H3-low  256))
	    (aset result 16 (/ sha1-H4-high 256))
	    (aset result 17 (% sha1-H4-high 256))
	    (aset result 18 (/ sha1-H4-low  256))
	    (aset result 19 (% sha1-H4-low  256))
	    result))
      ;; do not leave a copy of input string.
      (fillarray block-high nil)
      (fillarray block-low  nil))))

(defun sha1-string-internal (string &optional binary)
  (if binary
      (sha1-binary string)
    (encode-hex-string (sha1-binary string))))

(defun sha1-region-internal (beg end &optional binary)
  (sha1-string-internal (buffer-substring-no-properties beg end) binary))

;;;
;;; application interface.
;;;

(defun sha1-region (beg end &optional binary)
  (if (and sha1-use-external
	   sha1-maximum-internal-length
	   (> (abs (- end beg)) sha1-maximum-internal-length))
      (sha1-region-external beg end binary)
    (sha1-region-internal beg end binary)))

(defun sha1-string (string &optional binary)
  (if (and sha1-use-external
	   sha1-maximum-internal-length
	   (> (length string) sha1-maximum-internal-length))
      (sha1-string-external string binary)
    (sha1-string-internal string binary)))

;;;###autoload
(defun sha1 (object &optional beg end binary)
  "Return the SHA1 (Secure Hash Algorithm) of an object.
OBJECT is either a string or a buffer.
Optional arguments BEG and END denote buffer positions for computing the
hash of a portion of OBJECT.
If BINARY is non-nil, return a string in binary form."
  (if (stringp object)
      (sha1-string object binary)
    (with-current-buffer object
      (sha1-region (or beg (point-min)) (or end (point-max)) binary))))

(provide 'sha1)

;; arch-tag: c0f9abd0-ffc1-4557-aac6-ece7f2d4c901
;;; sha1.el ends here
