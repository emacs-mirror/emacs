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

;; 1-byte charsets.  Valid range of CHARSET-ID is 128..143.

;; CHARSET-ID 128 is not used.

(define-charset 129 'latin-iso8859-1
  [1 96 1 0 ?A 1 "RHP of Latin-1" "RHP of Latin-1 (ISO 8859-1): ISO-IR-100"
     "Right-Hand Part of Latin Alphabet 1 (ISO/IEC 8859-1): ISO-IR-100."])
(define-charset 130 'latin-iso8859-2
  [1 96 1 0 ?B 1 "RHP of Latin-2" "RHP of Latin-2 (ISO 8859-2): ISO-IR-101"
     "Right-Hand Part of Latin Alphabet 2 (ISO/IEC 8859-2): ISO-IR-101."])
(define-charset 131 'latin-iso8859-3
  [1 96 1 0 ?C 1 "RHP of Latin-3" "RHP of Latin-3 (ISO 8859-3): ISO-IR-109"
     "Right-Hand Part of Latin Alphabet 3 (ISO/IEC 8859-3): ISO-IR-109."])
(define-charset 132 'latin-iso8859-4
  [1 96 1 0 ?D 1 "RHP of Latin-4" "RHP of Latin-4 (ISO 8859-4): ISO-IR-110"
     "Right-Hand Part of Latin Alphabet 4 (ISO/IEC 8859-4): ISO-IR-110."])
(define-charset 133 'thai-tis620
  [1 96 1 0 ?T 1 "RHP of TIS620" "RHP of Thai (TIS620): ISO-IR-166"
     "Right-Hand Part of TIS620.2533 (Thai): ISO-IR-166."])
(define-charset 134 'greek-iso8859-7
  [1 96 1 0 ?F 1 "RHP of ISO8859/7" "RHP of Greek (ISO 8859-7): ISO-IR-126"
     "Right-Hand Part of Latin/Greek Alphabet (ISO/IEC 8859-7): ISO-IR-126."])
(define-charset 135 'arabic-iso8859-6
  [1 96 1 1 ?G 1 "RHP of ISO8859/6" "RHP of Arabic (ISO 8859-6): ISO-IR-127"
     "Right-Hand Part of Latin/Arabic Alphabet (ISO/IEC 8859-6): ISO-IR-127."])
(define-charset 136 'hebrew-iso8859-8
  [1 96 1 1 ?H 1 "RHP of ISO8859/8" "RHP of Hebrew (ISO 8859-8): ISO-IR-138"
     "Right-Hand Part of Latin/Hebrew Alphabet (ISO/IEC 8859-8): ISO-IR-138."])
(define-charset 137 'katakana-jisx0201
  [1 94 1 0 ?I 1 "JISX0201 Katakana" "Japanese Katakana (JISX0201.1976)"
     "Katakana Part of JISX0201.1976."])
(define-charset 138 'latin-jisx0201
  [1 94 1 0 ?J 0 "JISX0201 Roman" "Japanese Roman (JISX0201.1976)"
     "Roman Part of JISX0201.1976."])

;; CHARSET-ID is not used 139.

(define-charset 140 'cyrillic-iso8859-5
  [1 96 1 0 ?L 1 "RHP of ISO8859/5" "RHP of Cyrillic (ISO 8859-5): ISO-IR-144"
     "Right-Hand Part of Latin/Cyrillic Alphabet (ISO/IEC 8859-5): ISO-IR-144."])
(define-charset 141 'latin-iso8859-9
  [1 96 1 0 ?M 1 "RHP of Latin-5" "RHP of Latin-5 (ISO 8859-9): ISO-IR-148"
     "Right-Hand Part of Latin Alphabet 5 (ISO/IEC 8859-9): ISO-IR-148."])
(define-charset 142 'latin-iso8859-15
  [1 96 1 0 ?b 1 "RHP of Latin-9" "RHP of Latin-9 (ISO 8859-15): ISO-IR-203"
     "Right-Hand Part of Latin Alphabet 9 (ISO/IEC 8859-15): ISO-IR-203."])
(define-charset 143 'latin-iso8859-14
  [1 96 1 0 ?_ 1 "RHP of Latin-8" "RHP of Latin-8 (ISO 8859-14): ISO-IR-199"
     "Right-Hand Part of Latin Alphabet 8 (ISO/IEC 8859-14): ISO-IR-199."])

;; 2-byte charsets.  Valid range of CHARSET-ID is 144..153.

(define-charset 144 'japanese-jisx0208-1978
  [2 94 2 0 ?@ 0 "JISX0208.1978" "JISX0208.1978 (Japanese): ISO-IR-42"
     "JISX0208.1978 Japanese Kanji (so called \"old JIS\"): ISO-IR-42."])
(define-charset 145 'chinese-gb2312
  [2 94 2 0 ?A 0 "GB2312" "GB2312: ISO-IR-58"
     "GB2312 Chinese simplified: ISO-IR-58."])
(define-charset 146 'japanese-jisx0208
  [2 94 2 0 ?B 0 "JISX0208" "JISX0208.1983/1990 (Japanese): ISO-IR-87"
     "JISX0208.1983/1990 Japanese Kanji: ISO-IR-87."])
(define-charset 147 'korean-ksc5601
  [2 94 2 0 ?C 0 "KSC5601" "KSC5601 (Korean): ISO-IR-149"
     "KSC5601 Korean Hangul and Hanja: ISO-IR-149."])
(define-charset 148 'japanese-jisx0212
  [2 94 2 0 ?D 0 "JISX0212" "JISX0212 (Japanese): ISO-IR-159"
     "JISX0212 Japanese supplement: ISO-IR-159."])
(define-charset 149 'chinese-cns11643-1
  [2 94 2 0 ?G 0 "CNS11643-1" "CNS11643-1 (Chinese traditional): ISO-IR-171"
     "CNS11643 Plane 1 Chinese traditional: ISO-IR-171."])
(define-charset 150 'chinese-cns11643-2
  [2 94 2 0 ?H 0 "CNS11643-2" "CNS11643-2 (Chinese traditional): ISO-IR-172"
     "CNS11643 Plane 2 Chinese traditional: ISO-IR-172."])
(define-charset 151 'japanese-jisx0213-1
  [2 94 2 0 ?O 0 "JISX0213-1" "JISX0213-1" "JISX0213 Plane 1 (Japanese)"])
(define-charset 152 'chinese-big5-1
  [2 94 2 0 ?0 0 "Big5 (Level-1)" "Big5 (Level-1) A141-C67F"
     "Frequently used part (A141-C67F) of Big5 (Chinese traditional)."])
(define-charset 153 'chinese-big5-2
  [2 94 2 0 ?1 0 "Big5 (Level-2)" "Big5 (Level-2) C940-FEFE"
     "Less frequently used part (C940-FEFE) of Big5 (Chinese traditional)."])

;; Additional (private) character sets.  These character sets are
;; treated less space-efficiently in the buffer.

;; Syntax:
;; (define-charset CHARSET-ID CHARSET
;;   [ DIMENSION CHARS WIDTH DIRECTION ISO-FINAL-CHAR ISO-GRAPHIC-PLANE
;;     SHORT-NAME LONG-NAME DESCRIPTION ])

;; ISO-2022 allows a use of character sets not registered in ISO with
;; final characters `0' (0x30) through `?' (0x3F).  Among them, Emacs
;; reserves `0' through `9' to support several private character sets.
;; The remaining final characters `:' through `?' are for users.

;; 1-byte 1-column charsets.  Valid range of CHARSET-ID is 160..223.

(define-charset 160 'chinese-sisheng
  [1 94 1 0 ?0 0 "SiSheng" "SiSheng (PinYin/ZhuYin)"
     "Sisheng characters (vowels with tone marks) for Pinyin/Zhuyin."])

;; IPA characters for phonetic symbols.
(define-charset 161 'ipa
  [1 96 1 0 ?0 1 "IPA" "IPA"
     "IPA (International Phonetic Association) characters."])

;; Vietnamese VISCII.  VISCII is 1-byte character set which contains
;; more than 96 characters.  Since Emacs can't handle it as one
;; character set, it is divided into two: lower case letters and upper
;; case letters.
(define-charset 162 'vietnamese-viscii-lower
  [1 96 1 0 ?1 1 "VISCII lower" "VISCII lower-case"
     "Vietnamese VISCII1.1 lower-case characters."])
(define-charset 163 'vietnamese-viscii-upper
  [1 96 1 0 ?2 1 "VISCII upper" "VISCII upper-case"
     "Vietnamese VISCII1.1 upper-case characters."])

;; For Arabic, we need three different types of character sets.
;; Digits are of direction left-to-right and of width 1-column.
;; Others are of direction right-to-left and of width 1-column or
;; 2-column.
(define-charset 164 'arabic-digit
  [1 94 1 0 ?2 0 "Arabic digit" "Arabic digit"
     "Arabic digits."])
(define-charset 165 'arabic-1-column
  [1 94 1 1 ?3 0 "Arabic 1-col" "Arabic 1-column"
     "Arabic 1-column width glyphs."])

;; ASCII with right-to-left direction.
(define-charset 166 'ascii-right-to-left
  [1 94 1 1 ?B 0 "rev ASCII" "ASCII with right-to-left direction"
     "ASCII (left half of ISO 8859-1) with right-to-left direction."])

;; Lao script.
;; ISO10646's 0x0E80..0x0EDF are mapped to 0x20..0x7F.
(define-charset 167 'lao
  [1 94 1 0 ?1 0 "Lao" "Lao"
     "Lao characters (U+0E80..U+0EDF)."])

;; CHARSET-IDs 168..223 are not used.

;; 1-byte 2-column charsets.  Valid range of CHARSET-ID is 224..239.

(define-charset 224 'arabic-2-column
  [1 94 2 1 ?4 0 "Arabic 2-col" "Arabic 2-column"
     "Arabic 2-column glyphs."])

;; Indian scripts.  Symbolic charset for data exchange.  Glyphs are
;; not assigned.  They are automatically converted to each Indian
;; script which IS-13194 supports.

(define-charset 225 'indian-is13194
  [1 94 2 0 ?5 1 "IS 13194" "Indian IS 13194"
     "Generic Indian character set for data exchange with IS 13194."])

;; CHARSET-IDs 226..239 are not used.

(define-charset 240  'indian-glyph
  [2 96 1 0 ?4 0 "Indian glyph" "Indian glyph"
     "Glyphs for Indian characters."])
;; 240 used to be [2 94 1 0 ?6 0 "Indian 1-col" "Indian 1 Column"]

;; 2-byte 1-column charsets.  Valid range of CHARSET-ID is 240..244.

;; Actual Glyph for 1-column width.
(define-charset 241 'tibetan-1-column
  [2 94 1 0 ?8 0 "Tibetan 1-col" "Tibetan 1 column"
     "Tibetan 1-column glyphs."])

;; Subsets of Unicode.

(define-charset 242 'mule-unicode-2500-33ff
  [2 96 1 0 ?2 0 "Unicode subset 2" "Unicode subset (U+2500..U+33FF)"
     "Unicode characters of the range U+2500..U+33FF."])

(define-charset 243 'mule-unicode-e000-ffff
  [2 96 1 0 ?3 0 "Unicode subset 3" "Unicode subset (U+E000+FFFF)"
     "Unicode characters of the range U+E000..U+FFFF."])

(define-charset 244 'mule-unicode-0100-24ff
  [2 96 1 0 ?1 0 "Unicode subset" "Unicode subset (U+0100..U+24FF)"
     "Unicode characters of the range U+0100..U+24FF."])

;; 2-byte 2-column charsets.  Valid range of CHARSET-ID is 245..254.

;; Ethiopic characters (Amahric and Tigrigna).
(define-charset 245 'ethiopic
  [2 94 2 0 ?3 0 "Ethiopic" "Ethiopic characters"
     "Ethiopic characters."])

;; Chinese CNS11643 Plane3 thru Plane7.  Although these are official
;; character sets, the use is rare and don't have to be treated
;; space-efficiently in the buffer.
(define-charset 246 'chinese-cns11643-3
  [2 94 2 0 ?I 0 "CNS11643-3" "CNS11643-3 (Chinese traditional): ISO-IR-183"
     "CNS11643 Plane 3 Chinese Traditional: ISO-IR-183."])
(define-charset 247 'chinese-cns11643-4
  [2 94 2 0 ?J 0 "CNS11643-4" "CNS11643-4 (Chinese traditional): ISO-IR-184"
     "CNS11643 Plane 4 Chinese Traditional: ISO-IR-184."])
(define-charset 248 'chinese-cns11643-5
  [2 94 2 0 ?K 0 "CNS11643-5" "CNS11643-5 (Chinese traditional): ISO-IR-185"
     "CNS11643 Plane 5 Chinese Traditional: ISO-IR-185."])
(define-charset 249 'chinese-cns11643-6
  [2 94 2 0 ?L 0 "CNS11643-6" "CNS11643-6 (Chinese traditional): ISO-IR-186"
     "CNS11643 Plane 6 Chinese Traditional: ISO-IR-186."])
(define-charset 250 'chinese-cns11643-7
  [2 94 2 0 ?M 0 "CNS11643-7" "CNS11643-7 (Chinese traditional): ISO-IR-187"
     "CNS11643 Plane 7 Chinese Traditional: ISO-IR-187."])

;; Actual Glyph for 2-column width.
(define-charset 251 'indian-2-column
  [2 94 2 0 ?5 0 "Indian 2-col" "Indian 2 Column"
     "Indian character set for 2-column width glyphs."])
  ;; old indian-1-column characters will be translated to indian-2-column.
(declare-equiv-charset 2 94 ?6 'indian-2-column)

;; Tibetan script.
(define-charset 252 'tibetan
  [2 94 2 0 ?7 0 "Tibetan 2-col" "Tibetan 2 column"
     "Tibetan 2-column width glyphs."])

;; CHARSET-ID 253 is not used.

;; JISX0213 Plane 2
(define-charset 254 'japanese-jisx0213-2
  [2 94 2 0 ?P 0 "JISX0213-2" "JISX0213-2"
     "JISX0213 Plane 2 (Japanese)."])

;; Tell C code charset ID's of several charsets.
(setup-special-charsets)


;; These are tables for translating characters on decoding and
;; encoding.
(define-translation-table
  'oldjis-newjis-jisroman-ascii
  (list (cons (make-char 'japanese-jisx0208-1978)
	      (make-char 'japanese-jisx0208))
	(cons (make-char 'latin-jisx0201) (make-char 'ascii))))
(aset (get 'oldjis-newjis-jisroman-ascii 'translation-table)
      (make-char 'latin-jisx0201 92) (make-char 'latin-jisx0201 92))
(aset (get 'oldjis-newjis-jisroman-ascii 'translation-table)
      (make-char 'latin-jisx0201 126) (make-char 'latin-jisx0201 126))

(setq standard-translation-table-for-decode
      (get 'oldjis-newjis-jisroman-ascii 'translation-table))

(setq standard-translation-table-for-encode nil)

(defvar translation-table-for-input nil
  "If non-nil, a char table used to translate characters from input methods.
\(Currently only used by Quail.)")

;;; Make fundamental coding systems.

;; Miscellaneous coding systems which can't be made by
;; `make-coding-system'.

(put 'no-conversion 'coding-system
     (vector nil ?= "Do no conversion.

When you visit a file with this coding, the file is read into a
unibyte buffer as is, thus each byte of a file is treated as a
character."
	     (list 'coding-category 'coding-category-binary
		   'alias-coding-systems '(no-conversion))
	     nil))
(put 'no-conversion 'eol-type 0)
(put 'coding-category-binary 'coding-systems '(no-conversion))
(setq coding-system-list '(no-conversion))
(setq coding-system-alist '(("no-conversion")))
(register-char-codings 'no-conversion t)

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

(define-coding-system-alias 'unix 'undecided-unix)
(define-coding-system-alias 'dos 'undecided-dos)
(define-coding-system-alias 'mac 'undecided-mac)

;; Coding systems not specific to each language environment.

(make-coding-system
 'emacs-mule 0 ?=
 "Emacs internal format used in buffer and string.

Encoding text with this coding system produces the actual byte
sequence of the text in buffers and strings.  An exception is made for
eight-bit-control characters.  Each of them is encoded into a single
byte."
 nil
 '((safe-charsets . t)))

(make-coding-system
 'raw-text 5 ?t
 "Raw text, which means text contains random 8-bit codes.
Encoding text with this coding system produces the actual byte
sequence of the text in buffers and strings.  An exception is made for
eight-bit-control characters.  Each of them is encoded into a single
byte.

When you visit a file with this coding, the file is read into a
unibyte buffer as is (except for EOL format), thus each byte of a file
is treated as a character."
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
 'compound-text 2 ?x
 "Compound text based generic encoding for decoding unknown messages.

This coding system does not support ICCCM Extended Segments."
 '((ascii t) (latin-iso8859-1 katakana-jisx0201 t) t t
   nil ascii-eol ascii-cntl nil locking-shift single-shift nil nil nil
   init-bol nil nil)
 '((safe-charsets . t)
   (mime-charset . x-ctext)
   (composition . t)))

(define-coding-system-alias  'x-ctext 'compound-text)
(define-coding-system-alias  'ctext 'compound-text)

;; Same as compound-text, but doesn't produce composition escape
;; sequences.  Used in post-read and pre-write conversions of
;; compound-text-with-extensions, see mule.el.  Note that this should
;; not have a mime-charset property, to prevent it from showing up
;; close to the beginning of coding systems ordered by priority.
(make-coding-system
 'ctext-no-compositions 2 ?x
 "Compound text based generic encoding for decoding unknown messages.

Like `compound-text', but does not produce escape sequences for compositions."
 '((ascii t) (latin-iso8859-1 katakana-jisx0201 t) t t
   nil ascii-eol ascii-cntl nil locking-shift single-shift nil nil nil
   init-bol nil nil)
 '((safe-charsets . t)))

(make-coding-system
 'compound-text-with-extensions 5 ?x
 "Compound text encoding with ICCCM Extended Segment extensions.

This coding system should be used only for X selections.  It is inappropriate
for decoding and encoding files, process I/O, etc."
 nil
 '((post-read-conversion . ctext-post-read-conversion)
   (pre-write-conversion . ctext-pre-write-conversion)))

(define-coding-system-alias
  'x-ctext-with-extensions 'compound-text-with-extensions)
(define-coding-system-alias
  'ctext-with-extensions 'compound-text-with-extensions)

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
      '(("\\.elc\\'" . (emacs-mule . emacs-mule))
	("\\.utf\\(-8\\)?\\'" . utf-8)
	;; We use raw-text for reading loaddefs.el so that if it
	;; happens to have DOS or Mac EOLs, they are converted to
	;; newlines.  This is required to make the special treatment
	;; of the "\ newline" combination in loaddefs.el, which marks
	;; the beginning of a doc string, work.
	("\\(\\`\\|/\\)loaddefs.el\\'" . (raw-text . raw-text-unix))
	("\\.tar\\'" . (no-conversion . no-conversion))
	( "\\.po[tx]?\\'\\|\\.po\\." . po-find-file-coding-system)
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
      coding-category-utf-8		'mule-utf-8
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
   coding-category-utf-8
   coding-category-utf-16-be
   coding-category-utf-16-le))


;;; Miscellaneous settings.
(aset latin-extra-code-table ?\222 t)
(aset latin-extra-code-table ?\223 t)
(aset latin-extra-code-table ?\224 t)

(update-coding-systems-internal)

;;; mule-conf.el ends here
