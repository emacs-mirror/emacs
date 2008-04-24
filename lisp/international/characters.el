;;; characters.el --- set syntax and category for multibyte characters

;; Copyright (C) 1997, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008
;;   Free Software Foundation, Inc.
;; Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
;;   2005, 2006, 2007, 2008
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number H14PRO021
;; Copyright (C) 2003
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number H13PRO009

;; Keywords: multibyte character, character set, syntax, category

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

;;; Code:

;;; Predefined categories.

;; For each character set.

(define-category ?a "ASCII graphic characters 32-126 (ISO646 IRV:1983[4/0])")
(define-category ?l "Latin")
(define-category ?t "Thai")
(define-category ?g "Greek")
(define-category ?b "Arabic")
(define-category ?w "Hebrew")
(define-category ?y "Cyrillic")
(define-category ?k "Japanese katakana")
(define-category ?r "Japanese roman")
(define-category ?c "Chinese")
(define-category ?j "Japanese")
(define-category ?h "Korean")
(define-category ?e "Ethiopic (Ge'ez)")
(define-category ?v "Vietnamese")
(define-category ?i "Indian")
(define-category ?o "Lao")
(define-category ?q "Tibetan")

;; For each group (row) of 2-byte character sets.

(define-category ?A "Alpha-numeric characters of 2-byte character sets")
(define-category ?C "Chinese (Han) characters of 2-byte character sets")
(define-category ?G "Greek characters of 2-byte character sets")
(define-category ?H "Japanese Hiragana characters of 2-byte character sets")
(define-category ?K "Japanese Katakana characters of 2-byte character sets")
(define-category ?N "Korean Hangul characters of 2-byte character sets")
(define-category ?Y "Cyrillic characters of 2-byte character sets")
(define-category ?I "Indian Glyphs")

;; For phonetic classifications.

(define-category ?0 "consonant")
(define-category ?1 "base (independent) vowel")
(define-category ?2 "upper diacritical mark (including upper vowel)")
(define-category ?3 "lower diacritical mark (including lower vowel)")
(define-category ?4 "combining tone mark")
(define-category ?5 "symbol")
(define-category ?6 "digit")
(define-category ?7 "vowel-modifying diacritical mark")
(define-category ?8 "vowel-signs")
(define-category ?9 "semivowel lower")

;; For filling.
(define-category ?| "While filling, we can break a line at this character.")

;; For indentation calculation.
(define-category ?\s
  "This character counts as a space for indentation purposes.")

;; Keep the following for `kinsoku' processing.  See comments in
;; kinsoku.el.
(define-category ?> "A character which can't be placed at beginning of line.")
(define-category ?< "A character which can't be placed at end of line.")

;; Combining
(define-category ?^ "Combining diacritic or mark")

;;; Setting syntax and category.

;; ASCII

;; All ASCII characters have the category `a' (ASCII) and `l' (Latin).
(modify-category-entry '(32 . 127) ?a)
(modify-category-entry '(32 . 127) ?l)

;; Deal with the CJK charsets first.  Since the syntax of blocks is
;; defined per charset, and the charsets may contain e.g. Latin
;; characters, we end up with the wrong syntax definitions if we're
;; not careful.

;; Chinese characters (Unicode)
(modify-category-entry '(#x2E80 . #x312F) ?|)
(modify-category-entry '(#x3190 . #x33FF) ?|)
(modify-category-entry '(#x3400 . #x9FAF) ?C)
(modify-category-entry '(#x3400 . #x9FAF) ?c)
(modify-category-entry '(#x3400 . #x9FAF) ?|)
(modify-category-entry '(#xF900 . #xFAFF) ?C)
(modify-category-entry '(#xF900 . #xFAFF) ?c)
(modify-category-entry '(#xF900 . #xFAFF) ?|)
(modify-category-entry '(#x20000 . #x2AFFF) ?|)
(modify-category-entry '(#x2F800 . #x2FFFF) ?|)


;; Chinese character set (GB2312)

(map-charset-chars #'modify-syntax-entry 'chinese-gb2312 "_" #x2121 #x217E)
(map-charset-chars #'modify-syntax-entry 'chinese-gb2312 "_" #x2221 #x227E)
(map-charset-chars #'modify-syntax-entry 'chinese-gb2312 "_" #x2921 #x297E)

(map-charset-chars #'modify-category-entry 'chinese-gb2312 ?c)
(map-charset-chars #'modify-category-entry 'chinese-gb2312 ?A #x2330 #x2339)
(map-charset-chars #'modify-category-entry 'chinese-gb2312 ?A #x2341 #x235A)
(map-charset-chars #'modify-category-entry 'chinese-gb2312 ?A #x2361 #x237A)
(map-charset-chars #'modify-category-entry 'chinese-gb2312 ?H #x2421 #x247E)
(map-charset-chars #'modify-category-entry 'chinese-gb2312 ?K #x2521 #x257E)
(map-charset-chars #'modify-category-entry 'chinese-gb2312 ?G #x2621 #x267E)
(map-charset-chars #'modify-category-entry 'chinese-gb2312 ?Y #x2721 #x277E)
(map-charset-chars #'modify-category-entry 'chinese-gb2312 ?C #x3021 #x7E7E)

;; Chinese character set (BIG5)

(map-charset-chars #'modify-category-entry 'big5 ?c)
(map-charset-chars #'modify-category-entry 'big5 ?C #xA259 #xA25F)
(map-charset-chars #'modify-category-entry 'big5 ?C #xA440 #xC67E)
(map-charset-chars #'modify-category-entry 'big5 ?C #xC940 #xF9DF)

;; Chinese character set (CNS11643)

(dolist (c '(chinese-cns11643-1 chinese-cns11643-2 chinese-cns11643-3
	     chinese-cns11643-4 chinese-cns11643-5 chinese-cns11643-6
	     chinese-cns11643-7))
  (map-charset-chars #'modify-category-entry c ?c)
  (if (eq c 'chinese-cns11643-1)
      (map-charset-chars #'modify-category-entry c ?C #x4421 #x7E7E)
    (map-charset-chars #'modify-category-entry c ?C)))

;; Japanese character set (JISX0201, JISX0208, JISX0212, JISX0213)

(map-charset-chars #'modify-category-entry 'katakana-jisx0201 ?k)

(map-charset-chars #'modify-category-entry 'latin-jisx0201 ?r)

(dolist (l '(katakana-jisx0201 japanese-jisx0208 japanese-jisx0212
			       japanese-jisx0213-1 japanese-jisx0213-2))
  (map-charset-chars #'modify-category-entry l ?j))

;; Unicode equivalents of JISX0201-kana
(let ((range '(#xff61 . #xff9f)))
  (modify-category-entry range  ?k)
  (modify-category-entry range ?j)
  (modify-category-entry range ?\|))

;; Katakana block
(let ((range '(#x30a0 . #x30ff)))
  ;; ?K is double width, ?k isn't specified
  (modify-category-entry range ?K)
  (modify-category-entry range ?\|))

;; Hiragana block
(let ((range '(#x3040 . #x309d)))
  ;; ?H is actually defined to be double width
  ;;(modify-category-entry range ?H)
  (modify-category-entry range ?\|)
  )

;; JISX0208
(map-charset-chars #'modify-syntax-entry 'japanese-jisx0208 "_" #x2121 #x227E)
(map-charset-chars #'modify-syntax-entry 'japanese-jisx0208 "_" #x2821 #x287E)
(let ((chars '(?ー ?゛ ?゜ ?ヽ ?ヾ ?ゝ ?ゞ ?〃 ?仝 ?々 ?〆 ?〇)))
  (dolist (elt chars)
    (modify-syntax-entry (car chars) "w")))

(map-charset-chars #'modify-category-entry 'japanese-jisx0208 ?A #x2321 #x237E)
(map-charset-chars #'modify-category-entry 'japanese-jisx0208 ?H #x2421 #x247E)
(map-charset-chars #'modify-category-entry 'japanese-jisx0208 ?K #x2521 #x257E)
(map-charset-chars #'modify-category-entry 'japanese-jisx0208 ?G #x2621 #x267E)
(map-charset-chars #'modify-category-entry 'japanese-jisx0208 ?Y #x2721 #x277E)
(map-charset-chars #'modify-category-entry 'japanese-jisx0208 ?C #x3021 #x7E7E)
(modify-category-entry ?ー ?K)
(let ((chars '(?゛ ?゜)))
  (while chars
    (modify-category-entry (car chars) ?K)
    (modify-category-entry (car chars) ?H)
    (setq chars (cdr chars))))
(let ((chars '(?ヽ ?ヾ ?ゝ ?ゞ ?〃 ?仝 ?々 ?〆 ?〇)))
  (while chars
    (modify-category-entry (car chars) ?C)
    (setq chars (cdr chars))))

;; JISX0212

(map-charset-chars #'modify-syntax-entry 'japanese-jisx0212 "_" #x2121 #x237E)

;; JISX0201-Kana

(let ((chars '(?｡ ?､ ?･)))
  (while chars
    (modify-syntax-entry (car chars) ".")
    (setq chars (cdr chars))))

(modify-syntax-entry ?\｢ "(｣")
(modify-syntax-entry ?\｣ "(｢")

;; Korean character set (KSC5601)

(map-charset-chars #'modify-category-entry 'korean-ksc5601 ?h)

(map-charset-chars #'modify-syntax-entry 'korean-ksc5601 "_" #x2121 #x227E)
(map-charset-chars #'modify-syntax-entry 'korean-ksc5601 "_" #x2621 #x277E)
(map-charset-chars #'modify-syntax-entry 'korean-ksc5601 "_" #x2830 #x287E)
(map-charset-chars #'modify-syntax-entry 'korean-ksc5601 "_" #x2930 #x297E)
(map-charset-chars #'modify-category-entry 'korean-ksc5601 ?A #x2330 #x2339)
(map-charset-chars #'modify-category-entry 'korean-ksc5601 ?A #x2341 #x235A)
(map-charset-chars #'modify-category-entry 'korean-ksc5601 ?A #x2361 #x237A)
(map-charset-chars #'modify-category-entry 'korean-ksc5601 ?G #x2521 #x257E)
(map-charset-chars #'modify-category-entry 'korean-ksc5601 ?H #x2A21 #x2A7E)
(map-charset-chars #'modify-category-entry 'korean-ksc5601 ?K #x2B21 #x2B7E)
(map-charset-chars #'modify-category-entry 'korean-ksc5601 ?Y #x2C21 #x2C7E)

;; These are in more than one charset.
(let ((parens (concat "〈〉《》「」『』【】〔〕〖〗〘〙〚〛"
		      "︵︶︷︸︹︺︻︼︽︾︿﹀﹁﹂﹃﹄"
		      "（）［］｛｝"))
      open close)
  (dotimes (i (/ (length parens) 2))
    (setq open (aref parens (* i 2))
	  close (aref parens (1+ (* i 2))))
    (modify-syntax-entry open (format "(%c" close))
    (modify-syntax-entry close (format ")%c" open))))

;; Arabic character set

(let ((charsets '(arabic-iso8859-6
		  arabic-digit
		  arabic-1-column
		  arabic-2-column)))
  (while charsets
    (map-charset-chars #'modify-category-entry (car charsets) ?b)
    (setq charsets (cdr charsets))))
(modify-category-entry '(#x600 . #x6ff) ?b)
(modify-category-entry '(#xfb50 . #xfdff) ?b)
(modify-category-entry '(#xfe70 . #xfefe) ?b)

;; Cyrillic character set (ISO-8859-5)

(modify-syntax-entry ?№ ".")

;; Ethiopic character set

(modify-category-entry '(#x1200 . #x1399) ?e)
(modify-category-entry '(#x2d80 . #x2dde) ?e)
(let ((chars '(?፡ ?። ?፣ ?፤ ?፥ ?፦ ?፧ ?፨)))
  (while chars
    (modify-syntax-entry (car chars) ".")
    (setq chars (cdr chars))))
(map-charset-chars #'modify-category-entry 'ethiopic ?e)

;; Hebrew character set (ISO-8859-8)

(modify-syntax-entry #x5be ".") ; MAQAF
(modify-syntax-entry #x5c0 ".") ; PASEQ
(modify-syntax-entry #x5c3 ".") ; SOF PASUQ
(modify-syntax-entry #x5f3 ".") ; GERESH
(modify-syntax-entry #x5f4 ".") ; GERSHAYIM

;; Indian character set (IS 13194 and other Emacs original Indian charsets)

(modify-category-entry '(#x901 . #x970) ?i)
(map-charset-chars #'modify-category-entry 'indian-is13194 ?i)
(map-charset-chars #'modify-category-entry 'indian-2-column ?i)

;; Lao character set

(modify-category-entry '(#xe80 . #xeff) ?o)
(map-charset-chars #'modify-category-entry 'lao ?o)

(let ((deflist	'(("ກ-ຮ"	"w"	?0) ; consonant
		  ("ະາຳຽເ-ໄ"	"w"	?1) ; vowel base
		  ("ັິ-ືົໍ"	"w"	?2) ; vowel upper
		  ("ຸູ"	"w"	?3) ; vowel lower
		  ("່-໋"	"w"	?4) ; tone mark
		  ("ຼຽ"	"w"	?9) ; semivowel lower
		  ("໐-໙"	"w"	?6) ; digit
		  ("ຯໆ"	"_"	?5) ; symbol
		  ))
      elm chars len syntax category to ch i)
  (while deflist
    (setq elm (car deflist))
    (setq chars (car elm)
	  len (length chars)
	  syntax (nth 1 elm)
	  category (nth 2 elm)
	  i 0)
    (while (< i len)
      (if (= (aref chars i) ?-)
	  (setq i (1+ i)
		to (aref chars i))
	(setq ch (aref chars i)
	      to ch))
      (while (<= ch to)
	(unless (string-equal syntax "w")
	  (modify-syntax-entry ch syntax))
	(modify-category-entry ch category)
	(setq ch (1+ ch)))
      (setq i (1+ i)))
    (setq deflist (cdr deflist))))

;; Thai character set (TIS620)

(modify-category-entry '(#xe00 . #xe7f) ?t)
(map-charset-chars #'modify-category-entry 'thai-tis620 ?t)

(let ((deflist	'(;; chars	syntax	category
		  ("ก-รลว-ฮ"	"w"	?0) ; consonant
		  ("ฤฦะาำเ-ๅ"	"w"	?1) ; vowel base
		  ("ัิ-ื็๎"	"w"	?2) ; vowel upper
		  ("ุ-ฺ"	"w"	?3) ; vowel lower
		  ("่-ํ"	"w"	?4) ; tone mark
		  ("๐-๙"	"w"	?6) ; digit
		  ("ฯๆ฿๏๚๛"	"_"	?5) ; symbol
		  ))
      elm chars len syntax category to ch i)
  (while deflist
    (setq elm (car deflist))
    (setq chars (car elm)
	  len (length chars)
	  syntax (nth 1 elm)
	  category (nth 2 elm)
	  i 0)
    (while (< i len)
      (if (= (aref chars i) ?-)
	  (setq i (1+ i)
		to (aref chars i))
	(setq ch (aref chars i)
	      to ch))
      (while (<= ch to)
	(unless (string-equal syntax "w")
	  (modify-syntax-entry ch syntax))
	(modify-category-entry ch category)
	(setq ch (1+ ch)))
      (setq i (1+ i)))
    (setq deflist (cdr deflist))))

;; Tibetan character set

(modify-category-entry '(#xf00 . #xfff) ?q)
(map-charset-chars #'modify-category-entry 'tibetan ?q)
(map-charset-chars #'modify-category-entry 'tibetan-1-column ?q)

(let ((deflist	'(;; chars             syntax category
		  ("ཀ-ཀྵཪ"        	"w"	?0) ; consonant
		  ("ྐ-ྐྵྺྻྼ"       "w"     ?0) ;
		  ("ིེཻོཽྀ"       "w"	?2) ; upper vowel
		  ("ཾྂྃ྆྇ྈྉྊྋ" "w"	?2) ; upper modifier
		  ("྄ཱུ༙༵༷"       "w"	?3) ; lowel vowel/modifier
		  ("཰"		"w" ?3)		    ; invisible vowel a
		  ("༠-༩༪-༳"	        "w"	?6) ; digit
		  ("་།-༒༔ཿ"        "."     ?|) ; line-break char
		  ("་།༏༐༑༔ཿ"            "."     ?|) ;
		  ("༈་།-༒༔ཿ༽༴"  "."     ?>) ; prohibition
		  ("་།༏༐༑༔ཿ"            "."     ?>) ;
		  ("ༀ-༊༼࿁࿂྅"      "."     ?<) ; prohibition
		  ("༓༕-༘༚-༟༶༸-༻༾༿྾྿-࿏" "." ?q) ; others
		  ))
      elm chars len syntax category to ch i)
  (while deflist
    (setq elm (car deflist))
    (setq chars (car elm)
	  len (length chars)
	  syntax (nth 1 elm)
	  category (nth 2 elm)
	  i 0)
    (while (< i len)
      (if (= (aref chars i) ?-)
	  (setq i (1+ i)
		to (aref chars i))
	(setq ch (aref chars i)
	      to ch))
      (while (<= ch to)
	(unless (string-equal syntax "w")
	  (modify-syntax-entry ch syntax))
	(modify-category-entry ch category)
	(setq ch (1+ ch)))
      (setq i (1+ i)))
    (setq deflist (cdr deflist))))

;; Vietnamese character set

;; To make a word with Latin characters
(map-charset-chars #'modify-category-entry 'vietnamese-viscii-lower ?l)
(map-charset-chars #'modify-category-entry 'vietnamese-viscii-lower ?v)

(map-charset-chars #'modify-category-entry 'vietnamese-viscii-upper ?l)
(map-charset-chars #'modify-category-entry 'vietnamese-viscii-upper ?v)

(let ((tbl (standard-case-table))
      (i 32))
  (while (< i 128)
    (let* ((char (decode-char 'vietnamese-viscii-upper i))
	   (charl (decode-char 'vietnamese-viscii-lower i))
	   (uc (encode-char char 'ucs))
	   (lc (encode-char charl 'ucs)))
      (set-case-syntax-pair char (decode-char 'vietnamese-viscii-lower i)
			    tbl)	
      (if uc (modify-category-entry uc ?v))
      (if lc (modify-category-entry lc ?v)))
    (setq i (1+ i))))

;; Tai Viet
(let ((deflist '(;; chars	syntax	category
		 ((?ꪀ.  ?ꪯ)	"w"	?0) ; cosonant
		 ("ꪱꪵꪶ"		"w"	?1) ; vowel base
		 ((?ꪹ . ?ꪽ)	"w"	?1) ; vowel base
		 ("ꪰꪲꪳꪷꪸꪾ"	"w"	?2) ; vowel upper
		 ("ꪴ"		"w"	?3) ; vowel lower
		 ("ꫀꫂ"		"w"	?1) ; non-combining tone-mark
		 ("꪿꫁"		"w"	?4) ; combining tone-mark
		 ((?ꫛ . ?꫟)	"_"	?5) ; symbol
		 )))
  (dolist (elm deflist)
    (let ((chars (car elm))
	  (syntax (nth 1 elm))
	  (category (nth 2 elm)))
      (if (consp chars)
	  (progn
	    (modify-syntax-entry chars syntax)
	    (modify-category-entry chars category))
	(mapc #'(lambda (x)
		  (modify-syntax-entry x syntax)
		  (modify-category-entry x category))
	      chars)))))

;; Latin

(modify-category-entry '(#x80 . #x024F) ?l)

(let ((tbl (standard-case-table)) c)

  ;; Latin-1

  ;; Fixme: Some of the non-word syntaxes here perhaps should be
  ;; reviewed.  (Note that the following all implicitly have word
  ;; syntax: ¢£¤¥¨ª¯²³´¶¸¹º.)  There should be a well-defined way of
  ;; relating Unicode categories to Emacs syntax codes.

  ;; NBSP isn't semantically interchangeable with other whitespace chars,
  ;; so it's more like punctation.
  (set-case-syntax ?  "." tbl)
  (set-case-syntax ?¡ "." tbl)
  (set-case-syntax ?¦ "_" tbl)
  (set-case-syntax ?§ "." tbl)
  (set-case-syntax ?© "_" tbl)
  (set-case-syntax-delims 171 187 tbl)	; « »
  (set-case-syntax ?¬ "_" tbl)
  (set-case-syntax ?­ "_" tbl)
  (set-case-syntax ?® "_" tbl)
  (set-case-syntax ?° "_" tbl)
  (set-case-syntax ?± "_" tbl)
  (set-case-syntax ?µ "_" tbl)
  (set-case-syntax ?· "_" tbl)
  (set-case-syntax ?¼ "_" tbl)
  (set-case-syntax ?½ "_" tbl)
  (set-case-syntax ?¾ "_" tbl)
  (set-case-syntax ?¿ "." tbl)
  (let ((c 192))
    (while (<= c 222)
      (set-case-syntax-pair c (+ c 32) tbl)
      (setq c (1+ c))))
  (set-case-syntax ?× "_" tbl)
  (set-case-syntax ?ß "w" tbl)
  (set-case-syntax ?÷ "_" tbl)
  ;; See below for ÿ.

  ;; Latin Extended-A, Latin Extended-B
  (setq c #x0100)
  (while (<= c #x02B8)
    (modify-category-entry c ?l)
    (setq c (1+ c)))

  (let ((pair-ranges '((#x0100 . #x012F)
		       (#x0132 . #x0137)
		       (#x0139 . #x0148)
		       (#x014a . #x0177)
		       (#x0179 . #x017E)
		       (#x0182 . #x0185)
		       (#x0187 . #x018C)
		       (#x0191 . #x0192)
		       (#x0198 . #x0199)
		       (#x01A0 . #x01A5)
		       (#x01A7 . #x01A8)
		       (#x01AC . #x01AD)
		       (#x01AF . #x01B0)
		       (#x01B3 . #x01B6)
		       (#x01BC . #x01BD)
		       (#x01CD . #x01DC)
		       (#x01DE . #x01EF)
		       (#x01F4 . #x01F5)
		       (#x01F8 . #x021F)
		       (#x0222 . #x0233)
		       (#x023B . #x023C)
		       (#x0241 . #x0242)
		       (#x0246 . #x024F))))
    (dolist (elt pair-ranges)
      (let ((from (car elt)) (to (cdr elt)))
	(while (< from to)
	  (set-case-syntax-pair from (1+ from) tbl)
	  (setq from (+ from 2))))))

  ;; In some languages, such as Turkish, U+0049 LATIN CAPITAL LETTER I
  ;; and U+0131 LATIN SMALL LETTER DOTLESS I make a case pair, and so
  ;; do U+0130 LATIN CAPITAL LETTER I WITH DOT ABOVE and U+0069 LATIN
  ;; SMALL LETTER I.

  ;; We used to set up half of those correspondence unconditionally,
  ;; but that makes searches slow.  So now we don't set up either half
  ;; of these correspondences by default.

  ;; (set-downcase-syntax  ?İ ?i tbl)
  ;; (set-upcase-syntax    ?I ?ı tbl)

  (set-case-syntax-pair ?Ǆ ?ǆ tbl)
  (set-case-syntax-pair ?ǅ ?ǆ tbl)
  (set-case-syntax-pair ?Ǉ ?ǉ tbl)
  (set-case-syntax-pair ?ǈ ?ǉ tbl)
  (set-case-syntax-pair ?Ǌ ?ǌ tbl)
  (set-case-syntax-pair ?ǋ ?ǌ tbl)

  ;; 01F0; F; 006A 030C; # LATIN SMALL LETTER J WITH CARON
  (set-case-syntax-pair ?Ǳ ?ǳ tbl)
  (set-case-syntax-pair ?ǲ ?ǳ tbl)
  (set-case-syntax-pair ?Ƕ ?ƕ tbl)
  (set-case-syntax-pair ?Ƿ ?ƿ tbl)

  ;; Latin Extended Additional
  (modify-category-entry '(#x1e00 . #x1ef9) ?l)
  (setq c #x1e00)
  (while (<= c #x1ef9)
    (and (zerop (% c 2))
	 (or (<= c #x1e94) (>= c #x1ea0))
	 (set-case-syntax-pair c (1+ c) tbl))
    (setq c (1+ c)))

  ;; Greek
  (modify-category-entry '(#x0370 . #x03ff) ?g)
  (setq c #x0370)
  (while (<= c #x03ff)
    (if (or (and (>= c #x0391) (<= c #x03a1))
	    (and (>= c #x03a3) (<= c #x03ab)))
	(set-case-syntax-pair c (+ c 32) tbl))
    (and (>= c #x03da)
	 (<= c #x03ee)
	 (zerop (% c 2))
	 (set-case-syntax-pair c (1+ c) tbl))
    (setq c (1+ c)))
  (set-case-syntax-pair ?Ά ?ά tbl)
  (set-case-syntax-pair ?Έ ?έ tbl)
  (set-case-syntax-pair ?Ή ?ή tbl)
  (set-case-syntax-pair ?Ί ?ί tbl)
  (set-case-syntax-pair ?Ό ?ό tbl)
  (set-case-syntax-pair ?Ύ ?ύ tbl)
  (set-case-syntax-pair ?Ώ ?ώ tbl)

  ;; Armenian
  (setq c #x531)
  (while (<= c #x556)
    (set-case-syntax-pair c (+ c #x30) tbl)
    (setq c (1+ c)))

  ;; Greek Extended
  (modify-category-entry '(#x1f00 . #x1fff) ?g)
  (setq c #x1f00)
  (while (<= c #x1fff)
    (and (<= (logand c #x000f) 7)
	 (<= c #x1fa7)
	 (not (memq c '(#x1f50 #x1f52 #x1f54 #x1f56)))
	 (/= (logand c #x00f0) 7)
	 (set-case-syntax-pair (+ c 8) c tbl))
    (setq c (1+ c)))
  (set-case-syntax-pair ?Ᾰ ?ᾰ tbl)
  (set-case-syntax-pair ?Ᾱ ?ᾱ tbl)
  (set-case-syntax-pair ?Ὰ ?ὰ tbl)
  (set-case-syntax-pair ?Ά ?ά tbl)
  (set-case-syntax-pair ?ᾼ ?ᾳ tbl)
  (set-case-syntax-pair ?Ὲ ?ὲ tbl)
  (set-case-syntax-pair ?Έ ?έ tbl)
  (set-case-syntax-pair ?Ὴ ?ὴ tbl)
  (set-case-syntax-pair ?Ή ?ή tbl)
  (set-case-syntax-pair ?ῌ ?ῃ tbl)
  (set-case-syntax-pair ?Ῐ ?ῐ tbl)
  (set-case-syntax-pair ?Ῑ ?ῑ tbl)
  (set-case-syntax-pair ?Ὶ ?ὶ tbl)
  (set-case-syntax-pair ?Ί ?ί tbl)
  (set-case-syntax-pair ?Ῠ ?ῠ tbl)
  (set-case-syntax-pair ?Ῡ ?ῡ tbl)
  (set-case-syntax-pair ?Ὺ ?ὺ tbl)
  (set-case-syntax-pair ?Ύ ?ύ tbl)
  (set-case-syntax-pair ?Ῥ ?ῥ tbl)
  (set-case-syntax-pair ?Ὸ ?ὸ tbl)
  (set-case-syntax-pair ?Ό ?ό tbl)
  (set-case-syntax-pair ?Ὼ ?ὼ tbl)
  (set-case-syntax-pair ?Ώ ?ώ tbl)
  (set-case-syntax-pair ?ῼ ?ῳ tbl)

  ;; cyrillic
  (modify-category-entry '(#x0400 . #x04FF) ?y)
  (setq c #x0400)
  (while (<= c #x04ff)
    (and (>= c #x0400)
	 (<= c #x040f)
	 (set-case-syntax-pair c (+ c 80) tbl))
    (and (>= c #x0410)
	 (<= c #x042f)
	 (set-case-syntax-pair c (+ c 32) tbl))
    (and (zerop (% c 2))
	 (or (and (>= c #x0460) (<= c #x0480))
	     (and (>= c #x048c) (<= c #x04be))
	     (and (>= c #x04d0) (<= c #x04f4)))
	 (set-case-syntax-pair c (1+ c) tbl))
    (setq c (1+ c)))
  (set-case-syntax-pair ?Ӂ ?ӂ tbl)
  (set-case-syntax-pair ?Ӄ ?ӄ tbl)
  (set-case-syntax-pair ?Ӈ ?ӈ tbl)
  (set-case-syntax-pair ?Ӌ ?ӌ tbl)
  (set-case-syntax-pair ?Ӹ ?ӹ tbl)

  ;; general punctuation
  (setq c #x2000)
  (while (<= c #x200b)
    (set-case-syntax c " " tbl)
    (setq c (1+ c)))
  (while (<= c #x200F)
    (set-case-syntax c "." tbl)
    (setq c (1+ c)))
  ;; Fixme: These aren't all right:
  (setq c #x2010)
  (while (<= c #x2016)
    (set-case-syntax c "_" tbl)
    (setq c (1+ c)))
  ;; Punctuation syntax for quotation marks (like `)
  (while (<= c #x201f)
    (set-case-syntax  c "." tbl)
    (setq c (1+ c)))
  ;; Fixme: These aren't all right:
  (while (<= c #x2027)
    (set-case-syntax c "_" tbl)
    (setq c (1+ c)))
  (while (<= c #x206F)
    (set-case-syntax c "." tbl)
    (setq c (1+ c)))

  ;; Roman numerals
  (setq c #x2160)
  (while (<= c #x216f)
    (set-case-syntax-pair c (+ c #x10) tbl)
    (setq c (1+ c)))

  ;; Fixme: The following blocks might be better as symbol rather than
  ;; punctuation.
  ;; Arrows
  (setq c #x2190)
  (while (<= c #x21FF)
    (set-case-syntax c "." tbl)
    (setq c (1+ c)))
  ;; Mathematical Operators
  (while (<= c #x22FF)
    (set-case-syntax c "." tbl)
    (setq c (1+ c)))
  ;; Miscellaneous Technical
  (while (<= c #x23FF)
    (set-case-syntax c "." tbl)
    (setq c (1+ c)))
  ;; Control Pictures
  (while (<= c #x243F)
    (set-case-syntax c "_" tbl)
    (setq c (1+ c)))

  ;; Circled Latin
  (setq c #x24b6)
  (while (<= c #x24cf)
    (set-case-syntax-pair c (+ c 26) tbl)
    (modify-category-entry c ?l)
    (modify-category-entry (+ c 26) ?l)
    (setq c (1+ c)))

  ;; Fullwidth Latin
  (setq c #xff21)
  (while (<= c #xff3a)
    (set-case-syntax-pair c (+ c #x20) tbl)
    (modify-category-entry c ?l)
    (modify-category-entry (+ c #x20) ?l)
    (setq c (1+ c)))

  ;; Combining diacritics
  (modify-category-entry '(#x300 . #x362) ?^)
  ;; Combining marks
  (modify-category-entry '(#x20d0 . #x20e3) ?^)

  ;; Fixme: syntax for symbols &c
  )

(let ((pairs
       '("⁅⁆"				; U+2045 U+2046
	 "⁽⁾"				; U+207D U+207E
	 "₍₎"				; U+208D U+208E
	 "〈〉"				; U+2329 U+232A
	 "⎴⎵"				; U+23B4 U+23B5
	 "❨❩"				; U+2768 U+2769
	 "❪❫"				; U+276A U+276B
	 "❬❭"				; U+276C U+276D
	 "❰❱"				; U+2770 U+2771
	 "❲❳"				; U+2772 U+2773
	 "❴❵"				; U+2774 U+2775
	 "⟦⟧"				; U+27E6 U+27E7
	 "⟨⟩"				; U+27E8 U+27E9
	 "⟪⟫"				; U+27EA U+27EB
	 "⦃⦄"				; U+2983 U+2984
	 "⦅⦆"				; U+2985 U+2986
	 "⦇⦈"				; U+2987 U+2988
	 "⦉⦊"				; U+2989 U+298A
	 "⦋⦌"				; U+298B U+298C
	 "⦍⦎"				; U+298D U+298E
	 "⦏⦐"				; U+298F U+2990
	 "⦑⦒"				; U+2991 U+2992
	 "⦓⦔"				; U+2993 U+2994
	 "⦕⦖"				; U+2995 U+2996
	 "⦗⦘"				; U+2997 U+2998
	 "⧼⧽"				; U+29FC U+29FD
	 "〈〉"				; U+3008 U+3009
	 "《》"				; U+300A U+300B
	 "「」"				; U+300C U+300D
	 "『』"				; U+300E U+300F
	 "【】"				; U+3010 U+3011
	 "〔〕"				; U+3014 U+3015
	 "〖〗"				; U+3016 U+3017
	 "〘〙"				; U+3018 U+3019
	 "〚〛"				; U+301A U+301B
	 "﴾﴿"				; U+FD3E U+FD3F
	 "︵︶"				; U+FE35 U+FE36
	 "︷︸"				; U+FE37 U+FE38
	 "︹︺"				; U+FE39 U+FE3A
	 "︻︼"				; U+FE3B U+FE3C
	 "︽︾"				; U+FE3D U+FE3E
	 "︿﹀"				; U+FE3F U+FE40
	 "﹁﹂"				; U+FE41 U+FE42
	 "﹃﹄"				; U+FE43 U+FE44
	 "﹙﹚"				; U+FE59 U+FE5A
	 "﹛﹜"				; U+FE5B U+FE5C
	 "﹝﹞"				; U+FE5D U+FE5E
	 "（）"				; U+FF08 U+FF09
	 "［］"				; U+FF3B U+FF3D
	 "｛｝"				; U+FF5B U+FF5D
	 "｟｠"				; U+FF5F U+FF60
	 "｢｣"				; U+FF62 U+FF63
	 )))
  (dolist (elt pairs)
    (modify-syntax-entry (aref elt 0) (string ?\( (aref elt 1)))
    (modify-syntax-entry (aref elt 1) (string ?\) (aref elt 0)))))


;; For each character set, put the information of the most proper
;; coding system to encode it by `preferred-coding-system' property.

;; Fixme: should this be junked?
(let ((l '((latin-iso8859-1	. iso-latin-1)
	   (latin-iso8859-2	. iso-latin-2)
	   (latin-iso8859-3	. iso-latin-3)
	   (latin-iso8859-4	. iso-latin-4)
	   (thai-tis620		. thai-tis620)
	   (greek-iso8859-7	. greek-iso-8bit)
	   (arabic-iso8859-6	. iso-2022-7bit)
	   (hebrew-iso8859-8	. hebrew-iso-8bit)
	   (katakana-jisx0201	. japanese-shift-jis)
	   (latin-jisx0201	. japanese-shift-jis)
	   (cyrillic-iso8859-5	. cyrillic-iso-8bit)
	   (latin-iso8859-9	. iso-latin-5)
	   (japanese-jisx0208-1978 . iso-2022-jp)
	   (chinese-gb2312	. chinese-iso-8bit)
	   (chinese-gbk		. chinese-gbk)
	   (gb18030-2-byte	. chinese-gb18030)
	   (gb18030-4-byte-bmp	. chinese-gb18030)
	   (gb18030-4-byte-smp	. chinese-gb18030)
	   (gb18030-4-byte-ext-1 . chinese-gb18030)
	   (gb18030-4-byte-ext-2 . chinese-gb18030)
	   (japanese-jisx0208	. iso-2022-jp)
	   (korean-ksc5601	. iso-2022-kr)
	   (japanese-jisx0212	. iso-2022-jp)
	   (chinese-big5-1	. chinese-big5)
	   (chinese-big5-2	. chinese-big5)
	   (chinese-sisheng	. iso-2022-7bit)
	   (ipa			. iso-2022-7bit)
	   (vietnamese-viscii-lower . vietnamese-viscii)
	   (vietnamese-viscii-upper . vietnamese-viscii)
	   (arabic-digit	. iso-2022-7bit)
	   (arabic-1-column	. iso-2022-7bit)
	   (lao			. lao)
	   (arabic-2-column	. iso-2022-7bit)
	   (indian-is13194	. devanagari)
	   (indian-glyph	. devanagari)
	   (tibetan-1-column	. tibetan)
	   (ethiopic		. iso-2022-7bit)
	   (chinese-cns11643-1	. iso-2022-cn)
	   (chinese-cns11643-2	. iso-2022-cn)
	   (chinese-cns11643-3	. iso-2022-cn)
	   (chinese-cns11643-4	. iso-2022-cn)
	   (chinese-cns11643-5	. iso-2022-cn)
	   (chinese-cns11643-6	. iso-2022-cn)
	   (chinese-cns11643-7	. iso-2022-cn)
	   (indian-2-column	. devanagari)
	   (tibetan		. tibetan)
	   (latin-iso8859-14	. iso-latin-8)
	   (latin-iso8859-15	. iso-latin-9))))
  (while l
    (put-charset-property (car (car l)) 'preferred-coding-system (cdr (car l)))
    (setq l (cdr l))))


;; Setup auto-fill-chars for charsets that should invoke auto-filling.
;; SPACE and NEWLINE are already set.

(set-char-table-range auto-fill-chars '(#x3041 . #x30FF) t)
(set-char-table-range auto-fill-chars '(#x3400 . #x4DB5) t)
(set-char-table-range auto-fill-chars '(#x4e00 . #x9fbb) t)
(set-char-table-range auto-fill-chars '(#xF900 . #xFAFF) t)
(set-char-table-range auto-fill-chars '(#xFF00 . #xFF9F) t)
(set-char-table-range auto-fill-chars '(#x20000 . #x2FFFF) t)


;;; Setting char-width-table.  The default is 1.

;; 0: non-spacing, enclosing combining, formatting, Hangul Jamo medial
;;    and final characters.
(let ((l '((#x0300 . #x036F)
	   (#x0483 . #x0489)
	   (#x0591 . #x05BD)
	   (#x05BF . #x05BF)
	   (#x05C1 . #x05C2)
	   (#x05C4 . #x05C5)
	   (#x05C7 . #x05C7)
	   (#x0600 . #x0603)
	   (#x0610 . #x0615)
	   (#x064B . #x065E)
	   (#x0670 . #x0670)
	   (#x06D6 . #x06E4)
	   (#x06E7 . #x06E8)
	   (#x06EA . #x06ED)
	   (#x070F . #x070F)
	   (#x0711 . #x0711)
	   (#x0730 . #x074A)
	   (#x07A6 . #x07B0)
	   (#x07EB . #x07F3)
	   (#x0901 . #x0902)
	   (#x093C . #x093C)
	   (#x0941 . #x0948)
	   (#x094D . #x094D)
	   (#x0951 . #x0954)
	   (#x0962 . #x0963)
	   (#x0981 . #x0981)
	   (#x09BC . #x09BC)
	   (#x09C1 . #x09C4)
	   (#x09CD . #x09CD)
	   (#x09E2 . #x09E3)
	   (#x0A01 . #x0A02)
	   (#x0A3C . #x0A3C)
	   (#x0A41 . #x0A4D)
	   (#x0A70 . #x0A71)
	   (#x0A81 . #x0A82)
	   (#x0ABC . #x0ABC)
	   (#x0AC1 . #x0AC8)
	   (#x0ACD . #x0ACD)
	   (#x0AE2 . #x0AE3)
	   (#x0B01 . #x0B01)
	   (#x0B3C . #x0B3C)
	   (#x0B3F . #x0B3F)
	   (#x0B41 . #x0B43)
	   (#x0B4D . #x0B56)
	   (#x0B82 . #x0B82)
	   (#x0BC0 . #x0BC0)
	   (#x0BCD . #x0BCD)
	   (#x0C3E . #x0C40)
	   (#x0C46 . #x0C56)
	   (#x0CBC . #x0CBC)
	   (#x0CBF . #x0CBF)
	   (#x0CC6 . #x0CC6)
	   (#x0CCC . #x0CCD)
	   (#x0CE2 . #x0CE3)
	   (#x0D41 . #x0D43)
	   (#x0D4D . #x0D4D)
	   (#x0DCA . #x0DCA)
	   (#x0DD2 . #x0DD6)
	   (#x0E31 . #x0E31)
	   (#x0E34 . #x0E3A)
	   (#x0E47 . #x0E4E)
	   (#x0EB1 . #x0EB1)
	   (#x0EB4 . #x0EBC)
	   (#x0EC8 . #x0ECD)
	   (#x0F18 . #x0F19)
	   (#x0F35 . #x0F35)
	   (#x0F37 . #x0F37)
	   (#x0F39 . #x0F39)
	   (#x0F71 . #x0F7E)
	   (#x0F80 . #x0F84)
	   (#x0F86 . #x0F87)
	   (#x0F90 . #x0FBC)
	   (#x0FC6 . #x0FC6)
	   (#x102D . #x1030)
	   (#x1032 . #x1037)
	   (#x1039 . #x1039)
	   (#x1058 . #x1059)
	   (#x1160 . #x11FF)
	   (#x135F . #x135F)
	   (#x1712 . #x1714)
	   (#x1732 . #x1734)
	   (#x1752 . #x1753)
	   (#x1772 . #x1773)
	   (#x17B4 . #x17B5)
	   (#x17B7 . #x17BD)
	   (#x17C6 . #x17C6)
	   (#x17C9 . #x17D3)
	   (#x17DD . #x17DD)
	   (#x180B . #x180D)
	   (#x18A9 . #x18A9)
	   (#x1920 . #x1922)
	   (#x1927 . #x1928)
	   (#x1932 . #x1932)
	   (#x1939 . #x193B)
	   (#x1A17 . #x1A18)
	   (#x1B00 . #x1B03)
	   (#x1B34 . #x1B34)
	   (#x1B36 . #x1B3A)
	   (#x1B3C . #x1B3C)
	   (#x1B42 . #x1B42)
	   (#x1B6B . #x1B73)
	   (#x1DC0 . #x1DFF)
	   (#x200B . #x200F)
	   (#x202A . #x202E)
	   (#x2060 . #x206F)
	   (#x20D0 . #x20EF)
	   (#x302A . #x302F)
	   (#x3099 . #x309A)
	   (#xA806 . #xA806)
	   (#xA80B . #xA80B)
	   (#xA825 . #xA826)
	   (#xFB1E . #xFB1E)
	   (#xFE00 . #xFE0F)
	   (#xFE20 . #xFE23)
	   (#xFEFF . #xFEFF)
	   (#xFFF9 . #xFFFB)
	   (#x10A01 . #x10A0F)
	   (#x10A38 . #x10A3F)
	   (#x1D167 . #x1D169)
	   (#x1D173 . #x1D182)
	   (#x1D185 . #x1D18B)
	   (#x1D1AA . #x1D1AD)
	   (#x1D242 . #x1D244)
	   (#xE0001 . #xE01EF))))
  (dolist (elt l)
    (set-char-table-range char-width-table elt 0)))

;; 2: East Asian Wide and Full-width characters.
(let ((l '((#x1100 . #x115F)
	   (#x2329 . #x232A)
	   (#x2E80 . #x303E)
	   (#x3040 . #xA4CF)
	   (#xAC00 . #xD7A3)
	   (#xF900 . #xFAFF)
	   (#xFE30 . #xFE6F)
	   (#xFF01 . #xFF60)
	   (#xFFE0 . #xFFE6)
	   (#x20000 . #x2FFFF)
	   (#x30000 . #x3FFFF))))
  (dolist (elt l)
    (set-char-table-range char-width-table elt 2)))

;; Other double width
;;(map-charset-chars
;; (lambda (range ignore) (set-char-table-range char-width-table range 2))
;; 'ethiopic)
;; (map-charset-chars
;;  (lambda (range ignore) (set-char-table-range char-width-table range 2))
;; 'tibetan)
(map-charset-chars
 (lambda (range ignore) (set-char-table-range char-width-table range 2))
 'indian-2-column)
(map-charset-chars
 (lambda (range ignore) (set-char-table-range char-width-table range 2))
 'arabic-2-column)

(defvar cjk-char-width-table
  (let ((table (make-char-table nil)))
    (dolist (charset '(big5 chinese-gb2312 chinese-cns11643-1 
			    japanese-jisx0208 korean-ksc5601))
      (map-charset-chars #'(lambda (range arg)
			     (set-char-table-range table range 2))
			 charset))
    (optimize-char-table table)
    (set-char-table-parent table char-width-table)
    table)
  "Character width table used in CJK language environment.")

(defun use-cjk-char-width-table ()
  "Internal use only.
Setup char-width-table appropriate for CJK language environment."
  (setq char-width-table cjk-char-width-table))

(defun use-default-char-width-table ()
  "Internal use only.
Setup char-width-table appropriate for non-CJK language environment."
  (setq char-width-table (char-table-parent cjk-char-width-table)))

(optimize-char-table (standard-case-table))
(optimize-char-table (standard-category-table))
(optimize-char-table (standard-syntax-table))


;; Setting char-script-table.

;; The Unicode blocks actually extend past some of these ranges with
;; undefined codepoints.
(let ((script-list nil))
  (dolist
      (elt
       '((#x0000 #x007F latin)
	 (#x00A0 #x024F latin)
	 (#x0250 #x02AF phonetic)
	 (#x02B0 #x036F latin)
	 (#x0370 #x03E1 greek)
	 (#x03E2 #x03EF coptic)
	 (#x03F0 #x03F3 greek)
	 (#x0400 #x04FF cyrillic)
	 (#x0530 #x058F armenian)
	 (#x0590 #x05FF hebrew)
	 (#x0600 #x06FF arabic)
	 (#x0700 #x074F syriac)
	 (#x07C0 #x07FA nko)
	 (#x0780 #x07BF thaana)
	 (#x0900 #x097F devanagari)
	 (#x0980 #x09FF bengali)
	 (#x0A00 #x0A7F gurmukhi)
	 (#x0A80 #x0AFF gujarati)
	 (#x0B00 #x0B7F oriya)
	 (#x0B80 #x0BFF tamil)
	 (#x0C00 #x0C7F telugu)
	 (#x0C80 #x0CFF kannada)
	 (#x0D00 #x0D7F malayalam)
	 (#x0D80 #x0DFF sinhala)
	 (#x0E00 #x0E5F thai)
	 (#x0E80 #x0EDF lao)
	 (#x0F00 #x0FFF tibetan)
	 (#x1000 #x105F myanmar)
	 (#x10A0 #x10FF georgian)
	 (#x1100 #x11FF hangul)
	 (#x1200 #x139F ethiopic)
	 (#x13A0 #x13FF cherokee)
	 (#x1400 #x167F canadian-aboriginal)
	 (#x1680 #x169F ogham)
	 (#x16A0 #x16FF runic)
	 (#x1780 #x17FF khmer)
	 (#x1800 #x18AF mongolian)
	 (#x1D00 #x1DFF phonetic)
	 (#x1E00 #x1EFF latin)
	 (#x1F00 #x1FFF greek)
	 (#x2000 #x27FF symbol)
	 (#x2800 #x28FF braille)
	 (#x2D80 #x2DDF ethiopic)
	 (#x2E80 #x2FDF han)
	 (#x2FF0 #x2FFF ideographic-description)
	 (#x3000 #x303F cjk-misc)
	 (#x3040 #x30FF kana)
	 (#x3100 #x312F bopomofo)
	 (#x3130 #x318F hangul)
	 (#x3190 #x319F kanbun)
	 (#x31A0 #x31BF bopomofo)
	 (#x3400 #x9FAF han)
	 (#xA000 #xA4CF yi)
	 (#xAA00 #xAA5F cham)
	 (#xAA80 #xAADF tai-viet)
	 (#xAC00 #xD7AF hangul)
	 (#xF900 #xFAFF han)
	 (#xFB1D #xFB4F hebrew)
	 (#xFB50 #xFDFF arabic)
	 (#xFE70 #xFEFC arabic)
	 (#xFF00 #xFF5F cjk-misc)
	 (#xFF61 #xFF9F kana)
	 (#xFFE0 #xFFE6 cjk-misc)
	 (#x1D000 #x1D0FF byzantine-musical-symbol)
	 (#x1D100 #x1D1FF musical-symbol)
	 (#x1D400 #x1D7FF mathematical)
	 (#x20000 #x2AFFF han)
	 (#x2F800 #x2FFFF han)))
    (set-char-table-range char-script-table
			  (cons (car elt) (nth 1 elt)) (nth 2 elt))
    (or (memq (nth 2 elt) script-list)
	(setq script-list (cons (nth 2 elt) script-list))))
  (set-char-table-extra-slot char-script-table 0 (nreverse script-list)))

(map-charset-chars
 #'(lambda (range ignore)
     (set-char-table-range char-script-table range 'tibetan))
 'tibetan)


;;; Setting word boundary.

(defun next-word-boundary-han (pos limit)
  (if (<= pos limit)
      (save-excursion
	(goto-char pos)
	(looking-at "\\cC+")
	(goto-char (match-end 0))
	(if (looking-at "\\cH+")
	    (goto-char (match-end 0)))
	(point))
    (while (and (> pos limit)
		(eq (aref char-script-table (char-after (1- pos))) 'han))
      (setq pos (1- pos)))
    pos))

(defun next-word-boundary-kana (pos limit)
  (if (<= pos limit)
      (save-excursion
	(goto-char pos)
	(if (looking-at "\\cK+")
	    (goto-char (match-end 0)))
	(if (looking-at "\\cH+")
	    (goto-char (match-end 0)))
	(if (looking-at "\\ck+")
	    (goto-char (match-end 0)))
	(point))
    (let ((category-set (char-category-set (char-after pos)))
	  category)
      (if (or (aref category-set ?K) (aref category-set ?k))
	  (while (and (> pos limit)
		      (setq category-set 
			    (char-category-set (char-after (1- pos))))
		      (or (aref category-set ?K) (aref category-set ?k)))
	    (setq pos (1- pos)))
	(while (and (> pos limit)
		    (aref (setq category-set
				(char-category-set (char-after (1- pos)))) ?H))
	  (setq pos (1- pos)))
	(setq category (cond ((aref category-set ?C) ?C)
			     ((aref category-set ?K) ?K)
			     ((aref category-set ?A) ?A)))
	(when category
	  (setq pos (1- pos))
	  (while (and (> pos limit)
		      (aref (char-category-set (char-after (1- pos)))
			    category))
	    (setq pos (1- pos)))))
      pos)))

(map-char-table
 #'(lambda (char script)
     (cond ((eq script 'han)
	    (set-char-table-range find-word-boundary-function-table
				  char #'next-word-boundary-han))
	   ((eq script 'kana)
	    (set-char-table-range find-word-boundary-function-table
				  char #'next-word-boundary-kana))))
 char-script-table)

(setq word-combining-categories
      '((?l . ?l)
	(?C . ?C)
	(?C . ?H)
	(?C . ?K)))

(setq word-separating-categories	;  (2-byte character sets)
      '((?A . ?K)			; Alpha numeric - Katakana
	(?A . ?C)			; Alpha numeric - Chinese
	(?H . ?A)			; Hiragana - Alpha numeric
	(?H . ?K)			; Hiragana - Katakana
	(?H . ?C)			; Hiragana - Chinese
	(?K . ?A)			; Katakana - Alpha numeric
	(?K . ?C)			; Katakana - Chinese
	(?C . ?A)			; Chinese - Alpha numeric
	(?C . ?K)			; Chinese - Katakana
	))

;; Local Variables:
;; coding: utf-8
;; End:

;; arch-tag: 85889c35-9f4d-4912-9bf5-82de31b0d42d
;;; characters.el ends here
