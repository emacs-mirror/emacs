;;; mule-conf.el --- configure multilingual environment

;; Copyright (C) 1997 Electrotechnical Laboratory, JAPAN.
;; Licensed to the Free Software Foundation.

;; Keywords: mule, multilingual, character set, coding system

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

;; Don't byte-compile this file.

;;; Code:

;;; Definitions of character sets.

;; Basic (official) character sets.  These character sets are treated
;; efficiently with respect to buffer memory.

;; Syntax:
;; (define-charset CHARSET-ID CHARSET
;;   [ DIMENSION CHARS WIDTH DIRECTION ISO-FINAL-CHAR ISO-GRAPHIC-PLANE
;;     SHORT-NAME LONG-NAME DESCRIPTION ])
;; ASCII charset is defined in src/charset.c as below.
;; (define-charset 0 ascii
;;    [1 94 1 0 ?B 0 "ASCII" "ASCII" "ASCII (ISO646 IRV)"])

(define-charset 129 'latin-iso8859-1
  [1 96 1 0 ?A 1 "Latin-1" "ISO8859-1 (Latin-1)" "ISO8859-1 (Latin-1)"])
(define-charset 130 'latin-iso8859-2
  [1 96 1 0 ?B 1 "Latin-2" "ISO8859-2 (Latin-2)" "ISO8859-2 (Latin-2)"])
(define-charset 131 'latin-iso8859-3
  [1 96 1 0 ?C 1 "Latin-3" "ISO8859-3 (Latin-3)" "ISO8859-3 (Latin-3)"])
(define-charset 132 'latin-iso8859-4
  [1 96 1 0 ?D 1 "Latin-4" "ISO8859-4 (Latin-4)" "ISO8859-4 (Latin-4)"])
(define-charset 133 'thai-tis620
  [1 96 1 0 ?T 1 "TIS620" "TIS620 (Thai)" "TIS620.2529 (Thai)"])
(define-charset 134 'greek-iso8859-7
  [1 96 1 0 ?F 1 "ISO8859-7" "ISO8859-7 (Greek)" "ISO8859-7 (Greek)"])
(define-charset 135 'arabic-iso8859-6
  [1 96 1 1 ?G 1 "ISO8859-6" "ISO8859-6 (Arabic)" "ISO8859-6 (Arabic)"])
(define-charset 136 'hebrew-iso8859-8
  [1 96 1 1 ?H 1 "ISO8859-8" "ISO8859-8 (Hebrew)" "ISO8859-8 (Hebrew)"])
(define-charset 137 'katakana-jisx0201
  [1 94 1 0 ?I 1 "JISX0201 Kana" "JISX0201.1976 (Japanese Kana)"
     "JISX0201.1976 Japanese Kana"])
(define-charset 138 'latin-jisx0201
  [1 94 1 0 ?J 0 "JISX0201 Roman" "JISX0201.1976 (Japanese Roman)"
     "JISX0201.1976 Japanese Roman"])
(define-charset 140 'cyrillic-iso8859-5
  [1 96 1 0 ?L 1 "ISO8859-5" "ISO8859-5 (Cyrillic)"
     "ISO8859-5 (Cyrillic)"])
(define-charset 141 'latin-iso8859-9
  [1 96 1 0 ?M 1 "Latin-5" "ISO8859-9 (Latin-5)" "ISO8859-9 (Latin-5)"])
(define-charset 142 'latin-iso8859-15
  [1 96 1 0 ?b 1 "Latin-9" "ISO8859-15 (Latin-9)" "ISO8859-15 (Latin-9)"])
(define-charset 143 'latin-iso8859-14
  [1 96 1 0 ?_ 1 "Latin-8" "ISO8859-14 (Latin-8)" "ISO8859-14 (Latin-8)"])
(define-charset 144 'japanese-jisx0208-1978
  [2 94 2 0 ?@ 0 "JISX0208.1978" "JISX0208.1978 (Japanese)"
     "JISX0208.1978 Japanese Kanji (so called \"old JIS\")"])
(define-charset 145 'chinese-gb2312
  [2 94 2 0 ?A 0 "GB2312" "GB2312" "GB2312 Chinese simplified"])
(define-charset 146 'japanese-jisx0208
  [2 94 2 0 ?B 0 "JISX0208" "JISX0208.1983/1990 (Japanese)"
     "JISX0208.1983/1990 Japanese Kanji"])
(define-charset 147 'korean-ksc5601
  [2 94 2 0 ?C 0 "KSC5601" "KSC5601 (Korean)"
     "KSC5601 Korean Hangul and Hanja"])
(define-charset 148 'japanese-jisx0212
  [2 94 2 0 ?D 0 "JISX0212" "JISX0212 (Japanese)"
     "JISX0212 Japanese supplement"])
(define-charset 149 'chinese-cns11643-1
  [2 94 2 0 ?G 0 "CNS11643-1" "CNS11643-1 (Chinese traditional)"
     "CNS11643 Plane 1 Chinese traditional"])
(define-charset 150 'chinese-cns11643-2
  [2 94 2 0 ?H 0 "CNS11643-2" "CNS11643-2 (Chinese traditional)"
     "CNS11643 Plane 2 Chinese traditional"])
(define-charset 152 'chinese-big5-1
  [2 94 2 0 ?0 0 "Big5" "Big5 (Level-1)"
     "Big5 Level-1 Chinese traditional"])
(define-charset 153 'chinese-big5-2
  [2 94 2 0 ?1 0 "Big5" "Big5 (Level-2)"
     "Big5 Level-2 Chinese traditional"])

;; Additional (private) character sets.  These character sets are
;; treated less space-efficiently in the buffer.

;; Syntax:
;; (define-charset nil CHARSET
;;   [ DIMENSION CHARS WIDTH DIRECTION ISO-FINAL-CHAR ISO-GRAPHIC-PLANE
;;     SHORT-NAME LONG-NAME DESCRIPTION ])

;; ISO-2022 allows a use of character sets not registered in ISO with
;; final characters `0' (0x30) through `?' (0x3F).  Among them, Emacs
;; reserves `0' through `9' to support several private character sets.
;; The remaining final characters `:' through `?' are for users.

(define-charset nil 'chinese-sisheng
  [1 94 1 0 ?0 0 "SiSheng" "SiSheng (PinYin/ZhuYin)"
     "SiSheng characters for PinYin/ZhuYin"])

;; IPA characters for phonetic symbols.
(define-charset nil 'ipa
  [1 96 1 0 ?0 1 "IPA" "IPA" "IPA (International Phonetic Association)"])

;; Vietnamese VISCII.  VISCII is 1-byte character set which contains
;; more than 96 characters.  Since Emacs can't handle it as one
;; character set, it is divided into two: lower case letters and upper
;; case letters.
(define-charset nil 'vietnamese-viscii-lower
  [1 96 1 0 ?1 1 "VISCII lower" "VISCII lower-case" "VISCII1.1 lower-case"])
(define-charset nil 'vietnamese-viscii-upper
  [1 96 1 0 ?2 1 "VISCII upper" "VISCII upper-case" "VISCII1.1 upper-case"])

;; For Arabic, we need three different types of character sets.
;; Digits are of direction left-to-right and of width 1-column.
;; Others are of direction right-to-left and of width 1-column or
;; 2-column.
(define-charset nil 'arabic-digit
  [1 94 1 0 ?2 0 "Arabic digit" "Arabic digit" "Arabic digit"])
(define-charset nil 'arabic-1-column
  [1 94 1 1 ?3 0 "Arabic 1-col" "Arabic 1-column" "Arabic 1-column"])
(define-charset nil 'arabic-2-column
  [1 94 2 1 ?4 0 "Arabic 2-col" "Arabic 2-column" "Arabic 2-column"])

;; Ethiopic characters (Amahric and Tigrigna).
(define-charset nil 'ethiopic
  [2 94 2 0 ?3 0 "Ethiopic" "Ethiopic characters" "Ethiopic characters"])

;; Chinese CNS11643 Plane3 thru Plane7.  Although these are official
;; character sets, the use is rare and don't have to be treated
;; space-efficiently in the buffer.
(define-charset nil 'chinese-cns11643-3
  [2 94 2 0 ?I 0 "CNS11643-3" "CNS11643-3 (Chinese traditional)"
     "CNS11643 Plane 3 Chinese Traditional"])
(define-charset nil 'chinese-cns11643-4
  [2 94 2 0 ?J 0 "CNS11643-4" "CNS11643-4 (Chinese traditional)"
     "CNS11643 Plane 4 Chinese Traditional"])
(define-charset nil 'chinese-cns11643-5
  [2 94 2 0 ?K 0 "CNS11643-5" "CNS11643-5 (Chinese traditional)"
     "CNS11643 Plane 5 Chinese Traditional"])
(define-charset nil 'chinese-cns11643-6
  [2 94 2 0 ?L 0 "CNS11643-6" "CNS11643-6 (Chinese traditional)"
     "CNS11643 Plane 6 Chinese Traditional"])
(define-charset nil 'chinese-cns11643-7
  [2 94 2 0 ?M 0 "CNS11643-7" "CNS11643-7 (Chinese traditional)"
     "CNS11643 Plane 7 Chinese Traditional"])

;; ASCII with right-to-left direction.
(define-charset nil 'ascii-right-to-left
  [1 94 1 1 ?B 0 "rev ASCII" "ASCII with right-to-left direction"
     "ASCII (left half of ISO8859-1) with right-to-left direction"])

;; Indian scripts.  Symbolic charset for data exchange.  Glyphs are
;; not assigned.  They are automatically converted to each Indian
;; script which IS-13194 supports.

(define-charset nil 'indian-is13194
  [1 94 2 0 ?5 1 "IS 13194" "Indian IS 13194"
     "Generic Indian charset for data exchange with IS 13194"])
;; Actual Glyph for 2-column width.
(define-charset nil 'indian-2-column
  [2 94 2 0 ?5 0 "Indian 2-col" "Indian 2 Column"
     "Indian charset for 2-column width glyphs"])
;; Actual Glyph for 1-column width.
(define-charset nil 'indian-1-column
  [2 94 1 0 ?6 0 "Indian 1-col" "Indian 1 Column"
     "Indian charset for 2-column width glyphs"])

;; Lao script.
;; ISO10646's 0x0E80..0x0EDF are mapped to 0x20..0x7F.
(define-charset nil 'lao
  [1 94 1 0 ?1 0 "Lao" "Lao" "Lao characters (ISO10646 0E80..0EDF)"])

;; Tibetan script.
(define-charset nil 'tibetan
  [2 94 2 0 ?7 0 "Tibetan 2-col" "Tibetan 2 column" "Tibetan characters"])
(define-charset nil 'tibetan-1-column
  [2 94 1 0 ?8 0 "Tibetan 1-col" "Tibetan 1 column" "Tibetan 1 column glyph"])

;; Tell C code charset ID's of several charsets.
(setup-special-charsets)


;; These are tables for translating characters on decoding and
;; encoding.
(define-translation-table
  'oldjis-newjis-jisroman-ascii
  (list (cons (make-char 'japanese-jisx0208-1978)
	      (make-char 'japanese-jisx0208))
	(cons (make-char 'latin-jisx0201) (make-char 'ascii))))

(setq standard-translation-table-for-decode
      (get 'oldjis-newjis-jisroman-ascii 'translation-table))

(setq standard-translation-table-for-encode nil)


;;; Make fundamental coding systems.

;; Miscellaneous coding systems which can't be made by
;; `make-coding-system'.

(put 'no-conversion 'coding-system
     (vector nil ?= "Do no conversion"
	     (list 'coding-category 'coding-category-binary
		   'alias-coding-systems '(no-conversion))
	     nil))
(put 'no-conversion 'eol-type 0)
(put 'coding-category-binary 'coding-systems '(no-conversion))
(setq coding-system-list '(no-conversion))
(setq coding-system-alist '(("no-conversion")))

(define-coding-system-alias 'binary 'no-conversion)

(put 'undecided 'coding-system
     (vector t ?- "No conversion on encoding, automatic conversion on decoding"
	     (list 'alias-coding-systems '(undecided)
		   'safe-charsets '(ascii))
	     nil))
(setq coding-system-list (cons 'undecided coding-system-list))
(setq coding-system-alist (cons '("undecided") coding-system-alist))
(put 'undecided 'eol-type
     (make-subsidiary-coding-system 'undecided))

(define-coding-system-alias 'dos 'undecided-dos)
(define-coding-system-alias 'mac 'undecided-mac)

;; Coding systems not specific to each language environment.

(make-coding-system
 'emacs-mule 0 ?=
 "Emacs internal format used in buffer and string"
 nil
 '((safe-charsets . t)))

(make-coding-system
 'raw-text 5 ?t
 "Raw text, which means text contains random 8-bit codes."
 nil
 '((safe-charsets . t)))

(make-coding-system
 'iso-2022-7bit 2 ?J
 "ISO 2022 based 7-bit encoding using only G0"
 '((ascii t) nil nil nil
   short ascii-eol ascii-cntl seven)
 '((safe-charsets . t)
   (composition . t)))

(make-coding-system
 'iso-2022-7bit-ss2 2 ?$
 "ISO 2022 based 7-bit encoding using SS2 for 96-charset"
 '((ascii t) nil t nil
   short ascii-eol ascii-cntl seven nil single-shift)
 '((safe-charsets . t)
   (composition . t)))

(make-coding-system
 'iso-2022-7bit-lock 2 ?&
 "ISO-2022 coding system using Locking-Shift for 96-charset"
 '((ascii t) t nil nil
   nil ascii-eol ascii-cntl seven locking-shift)
 '((safe-charsets . t)
   (composition . t)))

(define-coding-system-alias 'iso-2022-int-1 'iso-2022-7bit-lock)

(make-coding-system
 'iso-2022-7bit-lock-ss2 2 ?i
 "Mixture of ISO-2022-JP, ISO-2022-KR, and ISO-2022-CN"
 '((ascii t)
   (nil korean-ksc5601 chinese-gb2312 chinese-cns11643-1 t)
   (nil chinese-cns11643-2)
   (nil chinese-cns11643-3 chinese-cns11643-4 chinese-cns11643-5
	chinese-cns11643-6 chinese-cns11643-7)
   short ascii-eol ascii-cntl seven locking-shift single-shift nil nil nil
   init-bol)
 '((safe-charsets ascii japanese-jisx0208 japanese-jisx0208-1978 latin-jisx0201
		  korean-ksc5601 chinese-gb2312 chinese-cns11643-1
		  chinese-cns11643-2 chinese-cns11643-3 chinese-cns11643-4
		  chinese-cns11643-5 chinese-cns11643-6 chinese-cns11643-7)
   (composition . t)))

(define-coding-system-alias 'iso-2022-cjk 'iso-2022-7bit-lock-ss2)

(make-coding-system
 'iso-2022-8bit-ss2 2 ?@
 "ISO 2022 based 8-bit encoding using SS2 for 96-charset"
 '((ascii t) nil t nil
   nil ascii-eol ascii-cntl nil nil single-shift)
 '((safe-charsets . t)
   (composition . t)))

(make-coding-system
 'x-ctext 2 ?x
 "Compound text based generic encoding for decoding unknown messages."
 '((ascii t) (latin-iso8859-1 t) t t
   nil ascii-eol ascii-cntl nil locking-shift single-shift nil nil nil
   init-bol nil nil)
 '((safe-charsets . t)
   (mime-charset . x-ctext)
   (composition . t)))

(make-coding-system
 'iso-safe 2 ?-
 "Convert all characters but ASCII to `?'."
 '(ascii nil nil nil
   nil ascii-eol ascii-cntl nil nil nil nil nil nil nil nil t)
 '((safe-charsets ascii)))

;; Use iso-safe for terminal output if some other coding system is not
;; specified explicitly.
(set-safe-terminal-coding-system-internal 'iso-safe)

;; The other coding-systems are defined in each language specific
;; section of languages.el.

;; Normally, set coding system to `undecided' before reading a file.
;; Compiled Emacs Lisp files (*.elc) are not decoded at all,
;; but we regard them as containing multibyte characters.
;; Tar files are not decoded at all, but we treat them as raw bytes.

(setq file-coding-system-alist
      '(("\\.elc$" . (emacs-mule . emacs-mule))
	("\\(\\`\\|/\\)loaddefs.el$" . (no-conversion . no-conversion))
	("\\.tar$" . (no-conversion . no-conversion))
	("" . (undecided . nil))))


;;; Setting coding categories and their priorities.

;; This setting is just to read an Emacs Lisp source files which
;; contain multilingual text while dumping Emacs.  More appropriate
;; values are set by the command `set-language-environment' for each
;; language environment.

(setq coding-category-emacs-mule	'emacs-mule
      coding-category-sjis		'japanese-shift-jis
      coding-category-iso-7		'iso-2022-7bit
      coding-category-iso-7-tight	'iso-2022-jp
      coding-category-iso-8-1		'iso-latin-1
      coding-category-iso-8-2		'iso-latin-1
      coding-category-iso-7-else	'iso-2022-7bit-lock
      coding-category-iso-8-else	'iso-2022-8bit-ss2
      coding-category-ccl		nil
      coding-category-utf-8             nil
      coding-category-utf-16-be         nil
      coding-category-utf-16-le         nil
      coding-category-big5		'chinese-big5
      coding-category-raw-text		'raw-text
      coding-category-binary		'no-conversion)

(set-coding-priority
 '(coding-category-iso-8-1
   coding-category-iso-8-2
   coding-category-iso-7-tight
   coding-category-iso-7
   coding-category-iso-7-else
   coding-category-iso-8-else
   coding-category-emacs-mule
   coding-category-raw-text
   coding-category-sjis 
   coding-category-big5
   coding-category-ccl
   coding-category-binary
   coding-category-utf-16-be
   coding-category-utf-16-le
   coding-category-utf-8))


;;; Miscellaneous settings.
(aset latin-extra-code-table ?\222 t)

(update-coding-systems-internal)

;;; mule-conf.el ends here
