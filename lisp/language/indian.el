;;; indian.el --- Indian languages support -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 1997, 1999, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008
;;   Free Software Foundation, Inc.
;; Copyright (C) 2002, 2003, 2004, 2005, 2006, 2007, 2008
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number H14PRO021

;; Maintainer:  KAWABATA, Taichi <kawabata@m17n.org>
;; Keywords: 	multilingual, i18n, Indian

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
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

;; This file defines in-is13194 coding system and relationship between
;; indian-glyph character-set and various CDAC fonts.

;;; Code:

(define-coding-system 'in-is13194-devanagari
  "8-bit encoding for ASCII (MSB=0) and IS13194-Devanagari (MSB=1)."
  :coding-type 'iso-2022
  :mnemonic ?D
  :designation [ascii indian-is13194 nil nil]
  :charset-list '(ascii indian-is13194)
  :post-read-conversion 'in-is13194-post-read-conversion
  :pre-write-conversion 'in-is13194-pre-write-conversion)

(define-coding-system-alias 'devanagari 'in-is13194-devanagari)

(defvar indian-font-foundry 'cdac
  "Font foundry for Indian characters.
Currently supported foundries are `cdac' and `akruti'.")

(defvar indian-script-language-alist
  '((devanagari (hindi sanskrit) nil)
    (bengali (bengali assamese) nil)
    (gurmukhi (punjabi) nil)
    (gujarati (gujarati) nil)
    (oriya (oriya) nil)
    (tamil (tamil) nil)
    (telugu (telugu) nil)
    (kannada (kannada) nil)
    (malayalam (malayalam) nil))
  "Alist of Indian scripts vs the corresponding language list and font foundry.
Each element has this form:

  (SCRIPT LANGUAGE-LIST FONT-FOUNDRY)

SCRIPT is one of Indian script names.

LANGUAGE-LIST is a list of Indian langauge names SCRIPT is used for.
The list is in the priority order.

FONT-FOUNDRY is a font foundry representing a group of Indian
fonts.  If the value is nil, the value of `indian-font-foundry'
is used.")

(defconst indian-font-char-index-table
  '(					; for which language(s)
    ;; CDAC fonts
    (#x0000 . cdac:dv-ttsurekh)		; hindi, etc
    (#x0100 . cdac:sd-ttsurekh)		; sanskrit
    (#x0200 . cdac:bn-ttdurga)		; bengali
    (#x0300 . cdac:tm-ttvalluvar)	; tamil
    (#x0400 . cdac:tl-tthemalatha)	; telugu
    (#x0500 . cdac:as-ttdurga)		; assamese
    (#x0600 . cdac:or-ttsarala)		; oriya
    (#x0700 . cdac:kn-ttuma)		; kannada
    (#x0800 . cdac:ml-ttkarthika)	; malayalam
    (#x0900 . cdac:gj-ttavantika)	; gujarati
    (#x0A00 . cdac:pn-ttamar)		; punjabi

    ;; AKRUTI fonts
    (#x0B00 . akruti:dev)		; hindi, etc
    (#x0C00 . akruti:bng)		; bengali
    (#x0D00 . akruti:pnj)		; punjabi
    (#x0E00 . akruti:guj)		; gujarati
    (#x0F00 . akruti:ori)		; oriya
    (#x1000 . akruti:tml)		; tamil
    (#x1100 . akruti:tlg)		; telugu
    (#x1200 . akruti:knd)		; kannada
    (#x1300 . akruti:mal)		; malayalam
    )
  "Alist of indices of `indian-glyph' character vs Indian font identifiers.
Each element has this form: (INDEX . FONT-IDENTIFIER)

INDEX is an index number of the first character in the charset
`indian-glyph' assigned for glyphs in the font specified by
FONT-IDENTIFIER.  Currently FONT-IDENTIFIERs are defined for CDAC
and AKRUTI font groups.")

(defun indian-font-char (index font-identifier)
  "Return character of charset `indian-glyph' made from glyph index INDEX.
FONT-IDENTIFIER is an identifier of an Indian font listed in the
variable `indian-font-char-index-table'.  It specifies which
font INDEX is for."
  (if (or (< index 0) (> index 255))
      (error "Invalid glyph index: %d" index))
  (let ((start (car (rassq font-identifier indian-font-char-index-table))))
    (if (not start)
	(error "Unknown font identifier: %s" font-identifier))
    (setq index (+ start index))
    (make-char 'indian-glyph (+ (/ index 96) 32) (+ (% index 96) 32))))

;; Return a range of characters (cons of min and max character) of the
;; charset `indian-glyph' for displaying SCRIPT in LANGUAGE by a font
;; of FOUNDRY.

(defun indian-font-char-range (font-identifier)
  (cons (indian-font-char 0 font-identifier)
	(indian-font-char 255 font-identifier)))

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

(defvar indian-composable-pattern
  (make-char-table nil)
  "Char table of regexps for composable Indian character sequence.")

(let ((script-regexp-alist
       '((devanagari . "[\x900-\x9FF\x200C\x200D]+")
	 (bengali . "[\x980-\x9FF\x200C\x200D]+")
	 (gurmukhi . "[\xA00-\xA7F\x200C\x200D]+")
	 (gujarati . "[\xA80-\xAFF\x200C\x200D]+")
	 (oriya . "[\xB00-\xB7F\x200C\x200D]+")
	 (tamil . "[\xB80-\xBFF\x200C\x200D]+")
	 (telugu . "[\xC00-\xC7F\x200C\x200D]+")
	 (kannada . "[\xC80-\xCFF\x200C\x200D]+")
	 (malayalam . "[\xD00-\xD7F\x200C\x200D]+")
	 (sinhala . "[\xD80-\xDFF\x200C\x200D]+"))))
  (map-char-table #'(lambda (key val) 
		      (let ((slot (assq val script-regexp-alist)))
			(if slot
			    (set-char-table-range 
			     composition-function-table key
			     (list (cons (cdr slot) 'font-shape-text))))))
		  char-script-table))
					      

(provide 'indian)

;;; arch-tag: 83aa8fc7-7ee2-4364-a6e5-498f5e3b8c2f
;;; indian.el ends here
