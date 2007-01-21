;;; thai.el --- support for Thai -*- coding: iso-2022-7bit; no-byte-compile: t -*-

;; Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
;;   2005, 2006, 2007
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number H14PRO021
;; Copyright (C) 1997, 1998, 2000, 2001, 2002, 2003, 2004, 2005,
;;   2006, 2007  Free Software Foundation, Inc.

;; Keywords: multilingual, Thai

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; For Thai, the character set TIS620 is supported.

;;; Code:

(make-coding-system
 'thai-tis620 2 ?T
 "8-bit encoding for ASCII (MSB=0) and Thai TIS620 (MSB=1)."
 '(ascii thai-tis620 nil nil
   nil nil nil nil nil nil nil nil nil nil nil t)
 '((safe-charsets ascii thai-tis620)
   (mime-charset . tis-620)
   (post-read-conversion . thai-post-read-conversion)))

(define-coding-system-alias 'th-tis620 'thai-tis620)
(define-coding-system-alias 'tis620 'thai-tis620)
(define-coding-system-alias 'tis-620 'thai-tis620)

(set-language-info-alist
 "Thai" '((tutorial . "TUTORIAL.th")
	  (charset thai-tis620)
	  (coding-system thai-tis620)
	  (coding-priority thai-tis620)
	  (nonascii-translation . thai-tis620)
	  (input-method . "thai-kesmanee")
	  (unibyte-display . thai-tis620)
	  (features thai-util)
	  (setup-function . setup-thai-language-environment-internal)
	  (exit-function . exit-thai-language-environment-internal)
	  (sample-text
	   . (thai-compose-string
	      (copy-sequence "Thai (,T@RIRd7B(B)		,TJ0GQ1J04U1$0CQ1:(B, ,TJ0GQ1J04U10$h1P(B")))
	  (documentation . t)))


;; Register a function to compose Thai characters.
(let ((patterns '(("\\c0?\\(\\c2\\|\\c3\\|\\c4\\)+"
		   . thai-composition-function))))
  (aset composition-function-table (make-char 'thai-tis620) patterns)
  (dotimes (i (1+ (- #xe7f #xe00)))
    (aset composition-function-table (decode-char 'ucs (+ i #xe00)) patterns)))

(provide 'thai)

;;; arch-tag: c7eb0e91-4db0-4619-81f8-8762e7d51e15
;;; thai.el ends here
