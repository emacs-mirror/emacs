;;; rfc2104.el --- RFC2104 Hashed Message Authentication Codes

;; Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003, 2004,
;;   2005, 2006, 2007, 2008 Free Software Foundation, Inc.

;; Author: Simon Josefsson <jas@pdc.kth.se>
;; Keywords: mail

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; This is a quick'n'dirty, low performance, implementation of RFC2104.
;;;
;;; Example:
;;;
;;; (require 'md5)
;;; (rfc2104-hash 'md5 64 16 "Jefe" "what do ya want for nothing?")
;;; "750c783e6ab0b503eaa86e310a5db738"
;;;
;;; (require 'sha-1)
;;; (rfc2104-hash 'sha1-encode 64 20 "Jefe" "what do ya want for nothing?")
;;; "effcdf6ae5eb2fa2d27416d5f184df9c259a7c79"
;;;
;;; 64 is block length of hash function (64 for MD5 and SHA), 16 is
;;; resulting hash length (16 for MD5, 20 for SHA).
;;;
;;; Tested with Emacs 20.2 and XEmacs 20.3.
;;;
;;; Test case reference: RFC 2202.

;;; Release history:
;;;
;;; 1998-08-16  initial release posted to gnu.emacs.sources
;;; 1998-08-17  use append instead of char-list-to-string
;;; 1998-08-26  don't require hexl
;;; 1998-09-25  renamed from hmac.el to rfc2104.el, also renamed functions
;;; 1999-10-23  included in pgnus
;;; 2000-08-15  `rfc2104-hexstring-to-bitstring'
;;; 2000-05-12  added sha-1 example, added test case reference
;;; 2003-11-13  change rfc2104-hexstring-to-bitstring to ...-byte-list

;;; Code:

(eval-when-compile (require 'cl))

;; Magic character for inner HMAC round. 0x36 == 54 == '6'
(defconst rfc2104-ipad ?\x36)

;; Magic character for outer HMAC round. 0x5C == 92 == '\'
(defconst rfc2104-opad ?\x5C)

;; Not so magic character for padding the key. 0x00
(defconst rfc2104-zero ?\x00)

;; Alist for converting hex to decimal.
(defconst rfc2104-hex-alist
  '((?0 . 0)	      (?a . 10)	      (?A . 10)
    (?1 . 1)	      (?b . 11)	      (?B . 11)
    (?2 . 2)	      (?c . 12)	      (?C . 12)
    (?3 . 3)	      (?d . 13)	      (?D . 13)
    (?4 . 4)	      (?e . 14)	      (?E . 14)
    (?5 . 5)	      (?f . 15)	      (?F . 15)
    (?6 . 6)
    (?7 . 7)
    (?8 . 8)
    (?9 . 9)))

(defun rfc2104-hex-to-int (str)
  (if str
      (if (listp str)
	  (+ (* 16 (rfc2104-hex-to-int (cdr str)))
	     (cdr (assoc (car str) rfc2104-hex-alist)))
	(rfc2104-hex-to-int (reverse (append str nil))))
    0))

(defun rfc2104-hexstring-to-byte-list (str)
  (let (out)
    (while (< 0 (length str))
      (push (rfc2104-hex-to-int (substring str -2)) out)
      (setq str (substring str 0 -2)))
    out))

(defun rfc2104-hash (hash block-length hash-length key text)
  (let* (;; if key is longer than B, reset it to HASH(key)
	 (key (if (> (length key) block-length)
		  (funcall hash key) key))
	 (k_ipad (append key nil))
	 (k_opad (append key nil)))
    ;; zero pad k_ipad/k_opad
    (while (< (length k_ipad) block-length)
      (setq k_ipad (append k_ipad (list rfc2104-zero))))
    (while (< (length k_opad) block-length)
      (setq k_opad (append k_opad (list rfc2104-zero))))
    ;; XOR key with ipad/opad into k_ipad/k_opad
    (setq k_ipad (mapcar (lambda (c) (logxor c rfc2104-ipad)) k_ipad))
    (setq k_opad (mapcar (lambda (c) (logxor c rfc2104-opad)) k_opad))
    ;; perform outer hash
    (funcall hash
	     (encode-coding-string
	      (concat k_opad (rfc2104-hexstring-to-byte-list
			      ;; perform inner hash
			      (funcall hash (concat k_ipad text))))
	      'iso-latin-1))))

(provide 'rfc2104)

;;; arch-tag: cf671d5c-a45f-4a09-815e-704e59e43950
;;; rfc2104.el ends here
