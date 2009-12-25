;;; indian.el --- Indian languages support -*- coding: utf-8; -*-

;; Copyright (C) 1997, 1999, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009
;;   Free Software Foundation, Inc.
;; Copyright (C) 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number H14PRO021

;; Maintainer:  Kenichi Handa <handa@m17n.org>
;;		KAWABATA, Taichi <kawabata@m17n.org>
;; Keywords: 	multilingual, i18n, Indian

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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file contains definitions of Indian language environments, and
;; setups for displaying the scrtipts used there.

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

(set-language-info-alist
 "Devanagari" '((charset unicode)
		(coding-system utf-8)
		(coding-priority utf-8)
		(input-method . "dev-aiba")
		(documentation . "\
Such languages using Devanagari script as Hindi and Marathi
are supported in this language environment."))
 '("Indian"))

(set-language-info-alist
 "Bengali" '((charset unicode)
	     (coding-system utf-8)
	     (coding-priority utf-8)
	     (input-method . "bengali-itrans")
	     (documentation . "\
Such languages using Bengali script as Bengali and Assamese
are supported in this language environment."))
 '("Indian"))

(set-language-info-alist
 "Punjabi" '((charset unicode)
	      (coding-system utf-8)
	      (coding-priority utf-8)
	      (input-method . "punjabi-itrans")
	      (documentation . "\
North Indian language Punjabi is supported in this language environment."))
 '("Indian"))

(set-language-info-alist
 "Gujarati" '((charset unicode)
	      (coding-system utf-8)
	      (coding-priority utf-8)
	      (input-method . "gujarati-itrans")
	      (documentation . "\
North Indian language Gujarati is supported in this language environment."))
 '("Indian"))

(set-language-info-alist
 "Oriya" '((charset unicode)
	      (coding-system utf-8)
	      (coding-priority utf-8)
	      (input-method . "oriya-itrans")
	      (documentation . "\
Such languages using Oriya script as Oriya, Khonti, and Santali
are supported in this language environment."))
 '("Indian"))

(set-language-info-alist
 "Tamil" '((charset unicode)
	   (coding-system utf-8)
	   (coding-priority utf-8)
	   (input-method . "tamil-itrans")
	   (documentation . "\
South Indian Language Tamil is supported in this language environment."))
 '("Indian"))

(set-language-info-alist
 "Telugu" '((charset unicode)
	    (coding-system utf-8)
	    (coding-priority utf-8)
	    (input-method . "telugu-itrans")
	    (documentation . "\
South Indian Language Telugu is supported in this language environment."))
 '("Indian"))

(set-language-info-alist
 "Kannada" '((charset unicode)
	     (coding-system mule-utf-8)
	     (coding-priority mule-utf-8)
	     (input-method . "kannada-itrans")
	     (sample-text . "Kannada (ಕನ್ನಡ)	ನಮಸ್ಕಾರ")
	     (documentation . "\
Kannada language and script is supported in this language
environment.")) 
 '("Indian"))

(set-language-info-alist
 "Malayalam" '((charset unicode)
	       (coding-system utf-8)
	       (coding-priority utf-8)
	       (input-method . "malayalam-itrans")
	       (documentation . "\
South Indian language Malayalam is supported in this language environment."))
 '("Indian"))

;; Replace mnemonic characters in REGEXP according to TABLE.  TABLE is
;; an alist of (MNEMONIC-STRING . REPLACEMENT-STRING).

(defun indian-compose-regexp (regexp table)
  (let ((case-fold-search nil))
    (dolist (elt table)
      (setq regexp (replace-regexp-in-string (car elt) (cdr elt) regexp t t)))
    regexp))

(defconst devanagari-composable-pattern
  (let ((table
	 '(("V" . "[\u0904-\u0914\u0960-\u0961\u0972]") ; independent vowel
	   ("C" . "[\u0915-\u0939]")		 ; consonant
	   ("R" . "\u0930")			 ; RA
	   ("n" . "\u093C")			 ; NUKTA
	   ("H" . "\u094D")			 ; HALANT
	   ("m" . "\u093F")			 ; vowel sign (pre)
	   ("u" . "[\u0945-\u0948\u0955]")	 ; vowel sign (above)
	   ("b" . "[\u0941-\u0944\u0962-\u0963]") ; vowel sign (below)
	   ("p" . "[\u093E\u0940\u0949-\u094C]") ; vowel sign (post)
	   ("A" . "[\u0900-\u0902\u0953-\u0954]") ; vowel modifier (above)
	   ("a" . "\u0903")			 ; vowel modifier (post) 
	   ("S" . "\u0951")			 ; stress sign (above)
	   ("s" . "\u0952")			 ; stress sign (below)
	   ("J" . "\u200D")			 ; ZWJ
	   ("N" . "\u200C")			 ; ZWNJ
	   ("X" . "[\u0900-\u097F]"))))		 ; all coverage
    (indian-compose-regexp
     (concat
      ;; syllables with an independent vowel, or
      "\\(?:RH\\)?Vn?m?b?u?p?n?A?s?S?a?\\|"
      ;; consonant-based syllables, or
      "\\(?:Cn?J?HJ?\\)*Cn?\\(?:H[NJ]?\\|m?b?u?p?n?A?s?S?a?\\)\\|"
      ;; special consonant form, or
      "JHR\\|"
      ;; any other singleton characters
      "X")
     table))
  "Regexp matching a composable sequence of Devanagari characters.")

(defconst tamil-composable-pattern
  (concat
   "\\([அ-ஔ]\\)\\|"
   "[ஂஃ]\\|" ;; vowel modifier considered independent
   "\\(\\(?:\\(?:க்ஷ\\)\\|[க-ஹ]\\)[்ா-ௌ]?\\)\\|"
   "\\(ஷ்ரீ\\)")
  "Regexp matching a composable sequence of Tamil characters.")

(defconst kannada-composable-pattern
  (concat
   "\\([ಂ-ಔೠಌ]\\)\\|[ಃ]"
   "\\|\\("
   "\\(?:\\(?:[ಕ-ಹ]್\\)?\\(?:[ಕ-ಹ]್\\)?\\(?:[ಕ-ಹ]್\\)?[ಕ-ಹ]್\\)?"
   "[ಕ-ಹ]\\(?:್\\|[ಾ-್ೕೃ]?\\)?"
   "\\)")
  "Regexp matching a composable sequence of Kannada characters.")

(defconst malayalam-composable-pattern
  (let ((table
	 '(("V" . "[\u0D05-\u0D14\u0D60-\u0D61]") ; independent vowel
	   ("C" . "[\u0D15-\u0D39]")		  ; consonant 
	   ("m" . "[\u0D46-\u0D48\u0D4A-\u0D4C]") ; prebase matra
	   ("p" . "[\u0D3E-\u0D44\u0D57]")	  ; postbase matra
	   ("b" . "[\u0D62-\u0D63]")		  ; belowbase matra
	   ("a" . "[\u0D02-\u0D03]")		  ; abovebase sign
	   ("H" . "\u0D4D")			  ; virama sign 
	   ("N" . "\u200D")			  ; ZWJ
	   ("J" . "\u200C")			  ; ZWNJ
	   ("X" . "[\u0D00-\u0D7F]"))))		  ; all coverage
    (indian-compose-regexp
     (concat
      ;; syllables with an independent vowel, or
      "V\\(?:J?HC\\)?m?b?p?a?\\|"
      ;; consonant-based syllables, or
      "\\(?:CJ?HJ?\\)\\{0,4\\}C\\(?:H[NJ]?\\|m?b?p?a?\\)\\|"
      ;; special consonant form, or
      "JHC\\|"
      ;; any other singleton characters
      "X")
     table))
  "Regexp matching a composable sequence of Malayalam characters.")

(let ((script-regexp-alist
       `((devanagari . ,devanagari-composable-pattern)
	 (bengali . "[\x980-\x9FF\x200C\x200D]+")
	 (gurmukhi . "[\xA00-\xA7F\x200C\x200D]+")
	 (gujarati . "[\xA80-\xAFF\x200C\x200D]+")
	 (oriya . "[\xB00-\xB7F\x200C\x200D]+")
	 (tamil . "[\xB80-\xBFF\x200C\x200D]+")
	 (telugu . "[\xC00-\xC7F\x200C\x200D]+")
	 (kannada . "[\xC80-\xCFF\x200C\x200D]+")
	 (malayalam . ,malayalam-composable-pattern))))
  (map-char-table
   #'(lambda (key val)
       (let ((slot (assq val script-regexp-alist)))
	 (if slot
	     (set-char-table-range
	      composition-function-table key
	      (list (vector (cdr slot) 0 'font-shape-gstring))))))
   char-script-table))

(provide 'indian)

;; arch-tag: 83aa8fc7-7ee2-4364-a6e5-498f5e3b8c2f
;;; indian.el ends here
