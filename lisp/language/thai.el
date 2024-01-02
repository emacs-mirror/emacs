;;; thai.el --- support for Thai -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright (C) 1997-1998, 2000-2024 Free Software Foundation, Inc.
;; Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
;;   2005, 2006, 2007, 2008, 2009, 2010, 2011
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number H13PRO009
;; Copyright (C) 2005
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number H14PRO021

;; Keywords: multilingual, Thai, i18n

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; For Thai, the character set TIS620 is supported.

;;; Code:

(define-coding-system 'thai-tis620
  "8-bit encoding for ASCII (MSB=0) and Thai TIS620 (MSB=1)."
  :coding-type 'charset
  :mnemonic ?T
  :charset-list '(tis620-2533))

(define-coding-system-alias 'th-tis620 'thai-tis620)
(define-coding-system-alias 'tis620 'thai-tis620)
(define-coding-system-alias 'tis-620 'thai-tis620)

(set-language-info-alist
 "Thai" '((tutorial . "TUTORIAL.th")
	  (charset thai-tis620)
	  (coding-system thai-tis620 iso-8859-11 cp874)
	  (coding-priority thai-tis620)
	  (nonascii-translation . tis620-2533)
	  (input-method . "thai-kesmanee")
	  (unibyte-display . thai-tis620)
	  (features thai-util)
	  (setup-function . setup-thai-language-environment-internal)
	  (exit-function . exit-thai-language-environment-internal)
	  (sample-text
	   . (thai-compose-string
	      (copy-sequence "Thai (ภาษาไทย)		สวัสดีครับ, สวัสดีค่ะ")))
	  (documentation . t)))

(define-coding-system 'cp874
  "DOS codepage 874 (Thai)"
  :coding-type 'charset
  :mnemonic ?D
  :charset-list '(cp874)
  :mime-charset 'cp874)
(define-coding-system-alias 'ibm874 'cp874)

(define-coding-system 'iso-8859-11
  "ISO/IEC 8859/11 (Latin/Thai)
This is the same as `thai-tis620' with the addition of no-break-space."
  :coding-type 'charset
  :mnemonic ?*
  :mime-charset 'iso-8859-11 ; not actually registered as of 2002-05-24
  :charset-list '(iso-8859-11))

;; For automatic composition.
(let ((chars "ัิีึืฺุู็่้๊๋์ํ๎")
      (elt '(["[ก-ฯ].[่้๊๋์]?ำ?" 1 thai-composition-function]
	     [nil 0 thai-composition-function])))
  (dotimes (i (length chars))
    (aset composition-function-table (aref chars i) elt)))
(aset composition-function-table ?ำ '(["[ก-ฯ]." 1 thai-composition-function]))

;; Tai-Tham

(set-language-info-alist
 "Northern Thai" '((charset unicode)
		   (coding-system utf-8)
		   (coding-priority utf-8)
		   (sample-text .
		     "Northern Thai (ᨣᩣᩴᨾᩮᩬᩥᨦ / ᨽᩣᩈᩣᩃ᩶ᩣ᩠ᨶᨶᩣ)	ᩈ᩠ᩅᩢᩔ᩠ᨯᩦᨣᩕᩢ᩠ᨸ")
		   (documentation . t)))

;; From Richard Wordingham <richard.wordingham@ntlworld.com>:
(defvar tai-tham-composable-pattern
  (let ((table
         ;; C is letters, independent vowels, digits, punctuation and symbols.
         '(("C" . "[\u1A20-\u1A54\u1A80-\u1A89\u1A90-\u1A99\u1AA0-\u1AAD]")
           ("M" .           ; Marks, CGJ, ZWNJ, ZWJ
            "[\u0324\u034F\u0E49\u0E4A\u0E4B\u1A55-\u1A57\u1A59-\u1A5E\u1A61-\u1A7C\u1A7F\u200C\200D]")
           ("H" . "\u1A60") ; Sakot
           ("S" .           ; Marks commuting with sakot
            "[\u0E49-\u0E4B\u0EC9\u0ECB\u1A75-\u1A7C]")
           ("N" . "\u1A58"))) ; mai kang lai
        (basic-syllable "C\\(N*\\(M\\|HS*C\\)\\)*")
        (regexp "X\\(N\\(X\\)?\\)*H?")) ; where X is basic syllable
    (let ((case-fold-search nil))
      (setq regexp (replace-regexp-in-string "X" basic-syllable regexp t t))
      (dolist (elt table)
        (setq regexp (replace-regexp-in-string (car elt) (cdr elt)
                                               regexp t t))))
    regexp))

(let ((elt (list (vector tai-tham-composable-pattern 0 'font-shape-gstring)
		 )))
  (set-char-table-range composition-function-table '(#x1A20 . #x1A54) elt)
  (set-char-table-range composition-function-table '(#x1A80 . #x1A89) elt)
  (set-char-table-range composition-function-table '(#x1A90 . #x1A99) elt)
  (set-char-table-range composition-function-table '(#x1AA0 . #x1AAD) elt))

(provide 'thai)

;;; thai.el ends here
