;;; characters.el --- set syntax and category for multibyte characters

;; Copyright (C) 1995, 1997 Electrotechnical Laboratory, JAPAN.
;;   Licensed to the Free Software Foundation.
;; Copyright (C) 2001, 2002 Free Software Foundation, Inc.
;; Copyright (C) 2003
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number H13PRO009

;; Keywords: multibyte character, character set, syntax, category

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

;;; Code:

;;; Predefined categories.

;; For each character set.

(define-category ?a "ASCII")
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
(define-category ?4 "tone mark")
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

(modify-category-entry '(#x1200 . #x137c) ?e)
(let ((chars '(?፡ ?። ?፣ ?፤ ?፥ ?፦ ?፧ ?፨ ?���� ?���� ?���� ?���� ?���� ?����)))
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
		  ("ྐ-ྐྵྺྻྼ��������"       "w"     ?0) ;
		  ("����-����"              "w"     ?0) ;
		  ("����-����"              "w"     ?0) ;
		  ("ིེཻོཽྀ"       "w"	?2) ; upper vowel
		  ("ཾྂྃ྆྇ྈྉྊྋ" "w"	?2) ; upper modifier
		  ("༙����྄ཱུ༵༷"       "w"	?3) ; lowel vowel/modifier
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


;; Latin

(modify-category-entry '(#x80 . #x024F) ?l)

(let ((tbl (standard-case-table)) c)

;; In some languages, U+0049 LATIN CAPITAL LETTER I and U+0131 LATIN
;; SMALL LETTER DOTLESS I make a case pair, and so do U+0130 LATIN
;; CAPITAL LETTER I WITH DOT ABOVE and U+0069 LATIN SMALL LETTER I.
;; See the Turkish language environment.

  ;; Latin-1

  ;; Fixme: Some of the non-word syntaxes here perhaps should be
  ;; reviewed.  (Note that the following all implicitly have word
  ;; syntax: ¢£¤¥¨ª¯²³´¶¸¹º.)  There should be a well-defined way of
  ;; relating Unicode categories to Emacs syntax codes.
  (set-case-syntax ?  " " tbl)		; dubious
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
  (while (<= c #x0233)
    (and (or (<= c #x012e)
	     (and (>= c #x014a) (<= c #x0177)))
	 (zerop (% c 2))
	 (set-case-syntax-pair c (1+ c) tbl))
    (and (>= c #x013a)
	 (<= c #x0148)
	 (zerop (% c 2))
	 (set-case-syntax-pair (1- c) c tbl))
    (setq c (1+ c)))
  (set-case-syntax-pair ?Ĳ ?ĳ tbl)
  (set-case-syntax-pair ?Ĵ ?ĵ tbl)
  (set-case-syntax-pair ?Ķ ?ķ tbl)
  (set-case-syntax-pair ?Ÿ ?ÿ tbl)
  (set-case-syntax-pair ?Ź ?ź tbl)
  (set-case-syntax-pair ?Ż ?ż tbl)
  (set-case-syntax-pair ?Ž ?ž tbl)

  ;; Latin Extended-B
  (set-case-syntax-pair ?Ɓ ?ɓ tbl)
  (set-case-syntax-pair ?Ƃ ?ƃ tbl)
  (set-case-syntax-pair ?Ƅ ?ƅ tbl)
  (set-case-syntax-pair ?Ɔ ?ɔ tbl)
  (set-case-syntax-pair ?Ƈ ?ƈ tbl)
  (set-case-syntax-pair ?Ɖ ?ɖ tbl)
  (set-case-syntax-pair ?Ɗ ?ɗ tbl)
  (set-case-syntax-pair ?Ƌ ?ƌ tbl)
  (set-case-syntax-pair ?Ǝ ?ǝ tbl)
  (set-case-syntax-pair ?Ə ?ə tbl)
  (set-case-syntax-pair ?Ɛ ?ɛ tbl)
  (set-case-syntax-pair ?Ƒ ?ƒ tbl)
  (set-case-syntax-pair ?Ɠ ?ɠ tbl)
  (set-case-syntax-pair ?Ɣ ?ɣ tbl)
  (set-case-syntax-pair ?Ɩ ?ɩ tbl)
  (set-case-syntax-pair ?Ɨ ?ɨ tbl)
  (set-case-syntax-pair ?Ƙ ?ƙ tbl)
  (set-case-syntax-pair ?Ɯ ?ɯ tbl)
  (set-case-syntax-pair ?Ɲ ?ɲ tbl)
  (set-case-syntax-pair ?Ɵ ?ɵ tbl)
  (set-case-syntax-pair ?Ơ ?ơ tbl)
  (set-case-syntax-pair ?Ƣ ?ƣ tbl)
  (set-case-syntax-pair ?Ƥ ?ƥ tbl)
  (set-case-syntax-pair ?Ʀ ?ʀ tbl)
  (set-case-syntax-pair ?Ƨ ?ƨ tbl)
  (set-case-syntax-pair ?Ʃ ?ʃ tbl)
  (set-case-syntax-pair ?Ƭ ?ƭ tbl)
  (set-case-syntax-pair ?Ʈ ?ʈ tbl)
  (set-case-syntax-pair ?Ư ?ư tbl)
  (set-case-syntax-pair ?Ʊ ?ʊ tbl)
  (set-case-syntax-pair ?Ʋ ?ʋ tbl)
  (set-case-syntax-pair ?Ƴ ?ƴ tbl)
  (set-case-syntax-pair ?Ƶ ?ƶ tbl)
  (set-case-syntax-pair ?Ʒ ?ʒ tbl)
  (set-case-syntax-pair ?Ƹ ?ƹ tbl)
  (set-case-syntax-pair ?Ƽ ?ƽ tbl)
  (set-case-syntax-pair ?Ǆ ?ǆ tbl)
  (set-case-syntax-pair ?ǅ ?ǆ tbl)
  (set-case-syntax-pair ?Ǉ ?ǉ tbl)
  (set-case-syntax-pair ?ǈ ?ǉ tbl)
  (set-case-syntax-pair ?Ǌ ?ǌ tbl)
  (set-case-syntax-pair ?ǋ ?ǌ tbl)
  (set-case-syntax-pair ?Ǎ ?ǎ tbl)
  (set-case-syntax-pair ?Ǐ ?ǐ tbl)
  (set-case-syntax-pair ?Ǒ ?ǒ tbl)
  (set-case-syntax-pair ?Ǔ ?ǔ tbl)
  (set-case-syntax-pair ?Ǖ ?ǖ tbl)
  (set-case-syntax-pair ?Ǘ ?ǘ tbl)
  (set-case-syntax-pair ?Ǚ ?ǚ tbl)
  (set-case-syntax-pair ?Ǜ ?ǜ tbl)
  (set-case-syntax-pair ?Ǟ ?ǟ tbl)
  (set-case-syntax-pair ?Ǡ ?ǡ tbl)
  (set-case-syntax-pair ?Ǣ ?ǣ tbl)
  (set-case-syntax-pair ?Ǥ ?ǥ tbl)
  (set-case-syntax-pair ?Ǧ ?ǧ tbl)
  (set-case-syntax-pair ?Ǩ ?ǩ tbl)
  (set-case-syntax-pair ?Ǫ ?ǫ tbl)
  (set-case-syntax-pair ?Ǭ ?ǭ tbl)
  (set-case-syntax-pair ?Ǯ ?ǯ tbl)
  ;; 01F0; F; 006A 030C; # LATIN SMALL LETTER J WITH CARON
  (set-case-syntax-pair ?Ǳ ?ǳ tbl)
  (set-case-syntax-pair ?ǲ ?ǳ tbl)
  (set-case-syntax-pair ?Ǵ ?ǵ tbl)
  (set-case-syntax-pair ?Ƕ ?ƕ tbl)
  (set-case-syntax-pair ?Ƿ ?ƿ tbl)
  (set-case-syntax-pair ?Ǹ ?ǹ tbl)
  (set-case-syntax-pair ?Ǻ ?ǻ tbl)
  (set-case-syntax-pair ?Ǽ ?ǽ tbl)
  (set-case-syntax-pair ?Ǿ ?ǿ tbl)
  (set-case-syntax-pair ?Ȁ ?ȁ tbl)
  (set-case-syntax-pair ?Ȃ ?ȃ tbl)
  (set-case-syntax-pair ?Ȅ ?ȅ tbl)
  (set-case-syntax-pair ?Ȇ ?ȇ tbl)
  (set-case-syntax-pair ?Ȉ ?ȉ tbl)
  (set-case-syntax-pair ?Ȋ ?ȋ tbl)
  (set-case-syntax-pair ?Ȍ ?ȍ tbl)
  (set-case-syntax-pair ?Ȏ ?ȏ tbl)
  (set-case-syntax-pair ?Ȑ ?ȑ tbl)
  (set-case-syntax-pair ?Ȓ ?ȓ tbl)
  (set-case-syntax-pair ?Ȕ ?ȕ tbl)
  (set-case-syntax-pair ?Ȗ ?ȗ tbl)
  (set-case-syntax-pair ?Ș ?ș tbl)
  (set-case-syntax-pair ?Ț ?ț tbl)
  (set-case-syntax-pair ?Ȝ ?ȝ tbl)
  (set-case-syntax-pair ?Ȟ ?ȟ tbl)
  (set-case-syntax-pair ?Ȣ ?ȣ tbl)
  (set-case-syntax-pair ?Ȥ ?ȥ tbl)
  (set-case-syntax-pair ?Ȧ ?ȧ tbl)
  (set-case-syntax-pair ?Ȩ ?ȩ tbl)
  (set-case-syntax-pair ?Ȫ ?ȫ tbl)
  (set-case-syntax-pair ?Ȭ ?ȭ tbl)
  (set-case-syntax-pair ?Ȯ ?ȯ tbl)
  (set-case-syntax-pair ?Ȱ ?ȱ tbl)
  (set-case-syntax-pair ?Ȳ ?ȳ tbl)

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
       '("$,1sEsF(B"				; U+2045 U+2046
	 "$,1s}s~(B"				; U+207D U+207E
	 "$,1t-t.(B"				; U+208D U+208E
	 "$,1{){*(B"				; U+2329 U+232A
	 "$,1|T|U(B"				; U+23B4 U+23B5
	 "$,2&H&I(B"				; U+2768 U+2769
	 "$,2&J&K(B"				; U+276A U+276B
	 "$,2&L&M(B"				; U+276C U+276D
	 "$,2&P&Q(B"				; U+2770 U+2771
	 "$,2&R&S(B"				; U+2772 U+2773
	 "$,2&T&U(B"				; U+2774 U+2775
	 "$,2'f'g(B"				; U+27E6 U+27E7
	 "$,2'h'i(B"				; U+27E8 U+27E9
	 "$,2'j'k(B"				; U+27EA U+27EB
	 "$,2,#,$(B"				; U+2983 U+2984
	 "$,2,%,&(B"				; U+2985 U+2986
	 "$,2,',((B"				; U+2987 U+2988
	 "$,2,),*(B"				; U+2989 U+298A
	 "$,2,+,,(B"				; U+298B U+298C
	 "$,2,-,.(B"				; U+298D U+298E
	 "$,2,/,0(B"				; U+298F U+2990
	 "$,2,1,2(B"				; U+2991 U+2992
	 "$,2,3,4(B"				; U+2993 U+2994
	 "$,2,5,6(B"				; U+2995 U+2996
	 "$,2,7,8(B"				; U+2997 U+2998
	 "$,2-<-=(B"				; U+29FC U+29FD
	 "$,2=H=I(B"				; U+3008 U+3009
	 "$,2=J=K(B"				; U+300A U+300B
	 "$,2=L=M(B"				; U+300C U+300D
	 "$,2=N=O(B"				; U+300E U+300F
	 "$,2=P=Q(B"				; U+3010 U+3011
	 "$,2=T=U(B"				; U+3014 U+3015
	 "$,2=V=W(B"				; U+3016 U+3017
	 "$,2=X=Y(B"				; U+3018 U+3019
	 "$,2=Z=[(B"				; U+301A U+301B
	 "$,3m~m(B"				; U+FD3E U+FD3F
	 "$,3pUpV(B"				; U+FE35 U+FE36
	 "$,3pWpX(B"				; U+FE37 U+FE38
	 "$,3pYpZ(B"				; U+FE39 U+FE3A
	 "$,3p[p\(B"				; U+FE3B U+FE3C
	 "$,3p]p^(B"				; U+FE3D U+FE3E
	 "$,3p_p`(B"				; U+FE3F U+FE40
	 "$,3papb(B"				; U+FE41 U+FE42
	 "$,3pcpd(B"				; U+FE43 U+FE44
	 "$,3pypz(B"				; U+FE59 U+FE5A
	 "$,3p{p|(B"				; U+FE5B U+FE5C
	 "$,3p}p~(B"				; U+FE5D U+FE5E
	 "$,3rhri(B"				; U+FF08 U+FF09
	 "$,3s;s=(B"				; U+FF3B U+FF3D
	 "$,3s[s](B"				; U+FF5B U+FF5D
	 "$,3s_s`(B"				; U+FF5F U+FF60
	 "$,3sbsc(B"				; U+FF62 U+FF63
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
	   (chinese-gb2312	. cn-gb-2312)
	   (japanese-jisx0208	. iso-2022-jp)
	   (korean-ksc5601	. iso-2022-kr)
	   (japanese-jisx0212	. iso-2022-jp)
	   (chinese-cns11643-1	. iso-2022-cn)
	   (chinese-cns11643-2	. iso-2022-cn)
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
;; SPACE and NEWLINE are already set.  Also put `nospace-between-words'
;; property on the charsets.
(let ((l '(katakana-jisx0201
	   japanese-jisx0208 japanese-jisx0212
	   chinese-gb2312 chinese-big5-1 chinese-big5-2)))
  (while l
    ;;(aset auto-fill-chars (make-char (car l)) t)
    (put-charset-property (car l) 'nospace-between-words t)
    (setq l (cdr l))))


;; CJK double width characters.
(let ((l '((#x1100 . #x11FF)
	   (#x2E80 . #x9FAF)
	   (#xAC00 . #xD7AF)
	   (#xF900 . #xFAFF)
	   (#xFE30 . #xFE4F)
	   (#xFF00 . #xFF5F)
	   (#xFFE0 . #xFFEF)
	   (#x20000 . #x2AFFF)
	   (#x2F800 . #x2FFFF))))
  (dolist (elt l)
    (set-char-table-range char-width-table
			  (cons (car elt) (cdr elt))
			  2)))
;; Fixme: Doing this affects non-CJK characters through unification,
;; but presumably CJK users expect those characters to be
;; double-width when using these charsets.
;; (map-charset-chars
;;  #'(lambda (range ignore) (set-char-table-range char-width-table range 2))
;;  'japanese-jisx0208)
;; (map-charset-chars
;;  #'(lambda (range ignore) (set-char-table-range char-width-table range 2))
;;  'japanese-jisx0212)
;; (map-charset-chars
;;  #'(lambda (range ignore) (set-char-table-range char-width-table range 2))
;;  'japanese-jisx0213-1)
;; (map-charset-chars
;;  #'(lambda (range ignore) (set-char-table-range char-width-table range 2))
;;  'japanese-jisx0213-2)
;; (map-charset-chars
;;  (lambda (range ignore) (set-char-table-range char-width-table range 2))
;;  'korean-ksc5601)

;; Other double width
(map-charset-chars
 (lambda (range ignore) (set-char-table-range char-width-table range 2))
 'ethiopic)
(map-charset-chars
 (lambda (range ignore) (set-char-table-range char-width-table range 2))
 'tibetan)
(map-charset-chars
 (lambda (range ignore) (set-char-table-range char-width-table range 2))
 'indian-2-column)
(map-charset-chars
 (lambda (range ignore) (set-char-table-range char-width-table range 2))
 'arabic-2-column)

(optimize-char-table (standard-case-table))
(optimize-char-table char-width-table)
(optimize-char-table (standard-category-table))
(optimize-char-table (standard-syntax-table))

;; The Unicode blocks actually extend past some of these ranges with
;; undefined codepoints.
(let ((script-list nil))
  (dolist
      (elt
       '((#x0000 #x007F latin)
	 (#x00A0 #x036F latin)
	 (#x0370 #x03E1 greek)
	 (#x03E2 #x03EF coptic)
	 (#x03F0 #x03F3 greek)
	 (#x0400 #x04FF cyrillic)
	 (#x0530 #x058F armenian)
	 (#x0590 #x05FF hebrew)
	 (#x0600 #x06FF arabic)
	 (#x0700 #x074F syriac)
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
	 (#x1200 #x137F ethiopic)
	 (#x13A0 #x13FF cherokee)
	 (#x1400 #x167F canadian-aboriginal)
	 (#x1680 #x169F ogham)
	 (#x16A0 #x16FF runic)
	 (#x1780 #x17FF khmer)
	 (#x1800 #x18AF mongolian)
	 (#x1E00 #x1EFF latin)
	 (#x1F00 #x1FFF greek)
	 (#x2000 #x27FF symbol)
	 (#x2800 #x28FF braille)
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
	 (#xAC00 #xD7AF hangul)
	 (#xF900 #xFAFF han)
	 (#xFB1D #xFB4F hebrew)
	 (#xFB50 #xFDFF arabic)
	 (#xFE70 #xFEFC arabic)
	 (#xFF00 #xFF5F cjk-misc)
	 (#xFF61 #xFF9F kana)
	 (#xFFE0 #xFFE6 cjk-misc)
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
      '((?l . ?l)))

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

;;; Local Variables:
;;; coding: utf-8-emacs
;;; End:

;;; arch-tag: 85889c35-9f4d-4912-9bf5-82de31b0d42d
;;; characters.el ends here
