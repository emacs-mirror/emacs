;;; thai.el --- Support for Thai -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 1995 Electrotechnical Laboratory, JAPAN.
;; Licensed to the Free Software Foundation.

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
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; For Thai, the character set TIS620 is supported.

;;; Code:

(make-coding-system
 'thai-tis620 2 ?T
 "8-bit encoding for ASCII (MSB=0) and Thai TIS620 (MSB=1)"
 '(ascii thai-tis620 nil nil
   nil ascii-eol)
 '((safe-charsets ascii thai-tis620)
   (post-read-conversion . thai-post-read-conversion)
   (pre-write-conversion . thai-pre-write-conversion)))

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
	  (sample-text . "Thai (,T@RIRd7B(B)		,TJ(B0,TGQ(B1,TJ(B0,T4U(B1,T$(B0,TCQ(B1,T:(B, ,TJ(B0,TGQ(B1,TJ(B0,T4U(B10,T$h(B1,TP(B")
	  (documentation . t)))

;;; thai.el ends here
