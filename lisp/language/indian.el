;;; indian.el --- Indian languages support -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 1999, 2001 Free Software Foundation, Inc.

;; Maintainer:  KAWABATA, Taichi <batta@beige.ocn.ne.jp>
;; Keywords: 	multilingual, i18n, Indian

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

;; This file defines in-is13194 coding system and relationship between
;; indian-glyph character-set and various CDAC fonts.

;;; Code:

(define-coding-system 'in-is13194-devanagari
  "8-bit encoding for ASCII (MSB=0) and IS13194-Devanagari (MSB=1)."
  :coding-type 'iso-2022
  :mnemonic ?D
  :designation [ascii indian-is13194 nil nil]
  :charset-list '(ascii indian-is13194)
  :post-read-conversion 'in-is13194-devanagari-post-read-conversion
  :pre-write-conversion 'in-is13194-devanagari-pre-write-conversion)

(define-coding-system-alias 'devanagari 'in-is13194-devanagari)

(defvar indian-script-table
  '[
    devanagari
    sanskrit
    bengali
    tamil
    telugu
    assamese
    oriya
    kannada
    malayalam
    gujarati
    punjabi
    ]
  "Vector of Indian script names.")

(let ((len (length indian-script-table))
      (i 0))
  (while (< i len)
    (put (aref indian-script-table i) 'indian-glyph-code-offset (* 256 i))
    (setq i (1+ i))))

(defvar indian-default-script 'devanagari
  "Default script for Indian languages.
Each Indian language environment sets this value
to one of `indian-script-table' (which see).
The default value is `devanagari'.")

(define-ccl-program ccl-encode-indian-glyph-font
  `(0
    ;; Shorten (r1 = (((((r1 - 32) * 96) + r2) - 32) % 256))
    (r1 = ((((r1 * 96) + r2) - ,(+ (* 32 96) 32)) % 256))))

(setq font-ccl-encoder-alist
      (cons (cons "-CDAC" 'ccl-encode-indian-glyph-font)
	    font-ccl-encoder-alist))

(setq font-ccl-encoder-alist
      (cons '("ISO10646.*-1" . ccl-encode-unicode-font)
	    font-ccl-encoder-alist))

(provide 'indian)

;;; indian.el ends here
