;;; characters.el --- set syntax and category for multibyte characters

;; Copyright (C) 1995, 1997 Electrotechnical Laboratory, JAPAN.
;; Licensed to the Free Software Foundation.
;; Copyright (C) 2001, 2002 Free Software Foundation, Inc.
;; Copyright (C) 2001, 2002
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

;; This file contains multibyte characters.  Save this file always in
;; the coding system `iso-2022-7bit'.

;; This file does not define the syntax for Latin-N character sets;
;; those are defined by the files latin-N.el.

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
(define-category ? 
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

;; Arabic character set

(let ((charsets '(arabic-iso8859-6
		  arabic-digit
		  arabic-1-column
		  arabic-2-column)))
  (while charsets
;;     (modify-syntax-entry (make-char (car charsets)) "w")
    (map-charset-chars #'modify-category-entry (car charsets) ?b)
    (setq charsets (cdr charsets))))
(modify-category-entry '(#x600 . #x6ff) ?b)
(modify-category-entry '(#xfb50 . #xfdff) ?b)
(modify-category-entry '(#xfe70 . #xfefe) ?b)

;; Chinese character set (GB2312)

;; (modify-syntax-entry (make-char 'chinese-gb2312) "w")
;; (modify-syntax-entry (make-char 'chinese-gb2312 33) "_")
;; (modify-syntax-entry (make-char 'chinese-gb2312 34) "_")
;; (modify-syntax-entry (make-char 'chinese-gb2312 41) "_")
(modify-syntax-entry ?\〔 "(〕")
(modify-syntax-entry ?\〈 "(〉")
(modify-syntax-entry ?\《 "(》")
(modify-syntax-entry ?\「 "(」")
(modify-syntax-entry ?\『 "(』")
(modify-syntax-entry ?\〖 "(〗")
(modify-syntax-entry ?\【 "(】")
(modify-syntax-entry ?\〕 ")〔")
(modify-syntax-entry ?\〉 ")〈")
(modify-syntax-entry ?\》 ")《")
(modify-syntax-entry ?\」 ")「")
(modify-syntax-entry ?\』 ")『")
(modify-syntax-entry ?\〗 ")〖")
(modify-syntax-entry ?\】 ")【")

;; Fixme: should any Chinese stuff be re-instated?

;; (modify-category-entry (make-char 'chinese-gb2312) ?c)
;; (modify-category-entry (make-char 'chinese-gb2312) ?\|)
;; (modify-category-entry (make-char 'chinese-gb2312 35) ?A)
;; (modify-category-entry (make-char 'chinese-gb2312 36) ?H)
;; (modify-category-entry (make-char 'chinese-gb2312 37) ?K)
;; (modify-category-entry (make-char 'chinese-gb2312 38) ?G)
;; (modify-category-entry (make-char 'chinese-gb2312 39) ?Y)
;; (let ((row 48))
;;   (while (< row 127)
;;     (modify-category-entry (make-char 'chinese-gb2312 row) ?C)
;;     (setq row (1+ row))))

;; Chinese character set (BIG5)

;; (let ((generic-big5-1-char (make-char 'chinese-big5-1))
;;       (generic-big5-2-char (make-char 'chinese-big5-2)))
;;   (modify-syntax-entry generic-big5-1-char "w")
;;   (modify-syntax-entry generic-big5-2-char "w")

;;   (modify-category-entry generic-big5-1-char ?c)
;;   (modify-category-entry generic-big5-2-char ?c)

;;   (modify-category-entry generic-big5-1-char ?C)
;;   (modify-category-entry generic-big5-2-char ?C)

;;   (modify-category-entry generic-big5-1-char ?\|)
;;   (modify-category-entry generic-big5-2-char ?\|))


;; Chinese character set (CNS11643)

;; (let ((cns-list '(chinese-cns11643-1
;; 		  chinese-cns11643-2
;; 		  chinese-cns11643-3
;; 		  chinese-cns11643-4
;; 		  chinese-cns11643-5
;; 		  chinese-cns11643-6
;; 		  chinese-cns11643-7))
;;       generic-char)
;;   (while cns-list
;;     (setq generic-char (make-char (car cns-list)))
;;     (modify-syntax-entry generic-char "w")
;;     (modify-category-entry generic-char ?c)
;;     (modify-category-entry generic-char ?C)
;;     (modify-category-entry generic-char ?|)
;;     (setq cns-list (cdr cns-list))))

;; Cyrillic character set (ISO-8859-5)

(modify-syntax-entry ?№ ".")
(let ((tbl (standard-case-table)))
  (set-case-syntax-pair ?Ё ?ё tbl)
  (set-case-syntax-pair ?Ђ ?ђ tbl)
  (set-case-syntax-pair ?Ѓ ?ѓ tbl)
  (set-case-syntax-pair ?Є ?є tbl)
  (set-case-syntax-pair ?Ѕ ?ѕ tbl)
  (set-case-syntax-pair ?І ?і tbl)
  (set-case-syntax-pair ?Ї ?ї tbl)
  (set-case-syntax-pair ?Ј ?ј tbl)
  (set-case-syntax-pair ?Љ ?љ tbl)
  (set-case-syntax-pair ?Њ ?њ tbl)
  (set-case-syntax-pair ?Ћ ?ћ tbl)
  (set-case-syntax-pair ?Ќ ?ќ tbl)
  (set-case-syntax-pair ?Ў ?ў tbl)
  (set-case-syntax-pair ?Џ ?џ tbl)
  (set-case-syntax-pair ?А ?а tbl)
  (set-case-syntax-pair ?Б ?б tbl)
  (set-case-syntax-pair ?В ?в tbl)
  (set-case-syntax-pair ?Г ?г tbl)
  (set-case-syntax-pair ?Д ?д tbl)
  (set-case-syntax-pair ?Е ?е tbl)
  (set-case-syntax-pair ?Ж ?ж tbl)
  (set-case-syntax-pair ?З ?з tbl)
  (set-case-syntax-pair ?И ?и tbl)
  (set-case-syntax-pair ?Й ?й tbl)
  (set-case-syntax-pair ?К ?к tbl)
  (set-case-syntax-pair ?Л ?л tbl)
  (set-case-syntax-pair ?М ?м tbl)
  (set-case-syntax-pair ?Н ?н tbl)
  (set-case-syntax-pair ?О ?о tbl)
  (set-case-syntax-pair ?П ?п tbl)
  (set-case-syntax-pair ?Р ?р tbl)
  (set-case-syntax-pair ?С ?с tbl)
  (set-case-syntax-pair ?Т ?т tbl)
  (set-case-syntax-pair ?У ?у tbl)
  (set-case-syntax-pair ?Ф ?ф tbl)
  (set-case-syntax-pair ?Х ?х tbl)
  (set-case-syntax-pair ?Ц ?ц tbl)
  (set-case-syntax-pair ?Ч ?ч tbl)
  (set-case-syntax-pair ?Ш ?ш tbl)
  (set-case-syntax-pair ?Щ ?щ tbl)
  (set-case-syntax-pair ?Ъ ?ъ tbl)
  (set-case-syntax-pair ?Ы ?ы tbl)
  (set-case-syntax-pair ?Ь ?ь tbl)
  (set-case-syntax-pair ?Э ?э tbl)
  (set-case-syntax-pair ?Ю ?ю tbl)
  (set-case-syntax-pair ?Я ?я tbl))

;; Devanagari character set

;;; Commented out since the categories appear not to be used anywhere
;;; and word syntax is the default.
;; (let ((deflist	'(;; chars	syntax	category
;; 		  ("������������"	"w"	?7) ; vowel-modifying diacritical mark
;; 					    ; chandrabindu, anuswar, visarga
;; 		  ("����-����"	"w"	?1) ; independent vowel
;; 		  ("����-����"	"w"	?0) ; consonant
;; 		  ("����-����"	"w"	?8) ; matra
;; 		  ("����-����"	"w"	?6) ; digit
;; 		  ;; Unicode equivalents
;; 		  ("ँंः"	"w"	?7) ; vowel-modifying diacritical mark
;; 					    ; chandrabindu, anuswar, visarga
;; 		  ("अ-ऍ"	"w"	?1) ; independent vowel
;; 		  ("क-ह"	"w"	?0) ; consonant
;; 		  ("ा-ॉ"	"w"	?8) ; matra
;; 		  ("०-९"	"w"	?6) ; digit
;; 		  ))
;;       elm chars len syntax category to ch i)
;;   (while deflist
;;     (setq elm (car deflist))
;;     (setq chars (car elm)
;; 	  len (length chars)
;; 	  syntax (nth 1 elm)
;; 	  category (nth 2 elm)
;; 	  i 0)
;;     (while (< i len)
;;       (if (= (aref chars i) ?-)
;; 	  (setq i (1+ i)
;; 		to (aref chars i))
;; 	(setq ch (aref chars i)
;; 	      to ch))
;;       (while (<= ch to)
;; 	(modify-syntax-entry ch syntax)
;; 	(modify-category-entry ch category)
;; 	(setq ch (1+ ch)))
;;       (setq i (1+ i)))
;;     (setq deflist (cdr deflist))))

;; Ethiopic character set

(modify-category-entry '(#x1200 . #x137b) ?e)
(let ((chars '(?፡ ?። ?፣ ?፤ ?፥ ?፦ ?፧ ?፨ ?���� ?���� ?���� ?���� ?���� ?����)))
  (while chars
    (modify-syntax-entry (car chars) ".")
    (setq chars (cdr chars))))
(map-charset-chars #'modify-category-entry 'ethiopic ?e)

;; Greek character set (ISO-8859-7)

(modify-category-entry '(#x370 . #x3ff) ?g)

;; (let ((c 182))
;;   (while (< c 255)
;;     (modify-syntax-entry (make-char 'greek-iso8859-7 c) "w")
;;     (setq c (1+ c))))
;; (modify-syntax-entry (make-char 'greek-iso8859-7 160) "w") ; NBSP
;; (modify-syntax-entry ?· ".")
;; (modify-syntax-entry ?» ".")
;; (modify-syntax-entry ?½ ".")
(let ((tbl (standard-case-table)))
  ;; Fixme: non-letter syntax copied from latin-1, but that's dubious
  ;; in several cases.
  (set-case-syntax ?‘ "." tbl)
  (set-case-syntax ?’ "." tbl)
  (set-case-syntax ?¦ "." tbl)
  (set-case-syntax ?¦ "_" tbl)
  (set-case-syntax ?§ "." tbl)
  (set-case-syntax ?© "_" tbl)
  (set-case-syntax ?\« "." tbl)
  (set-case-syntax ?¬ "_" tbl)
  (set-case-syntax ?­ "_" tbl)
  (set-case-syntax ?― "." tbl)
  (set-case-syntax ?° "_" tbl)
  (set-case-syntax ?± "_" tbl)
;;  (set-case-syntax ?· "_" tbl)
;;  (set-case-syntax ?½ "_" tbl)
  (set-case-syntax-pair ?Α ?α tbl)
  (set-case-syntax-pair ?Β ?β tbl)
  (set-case-syntax-pair ?Γ ?γ tbl)
  (set-case-syntax-pair ?Δ ?δ tbl)
  (set-case-syntax-pair ?Ε ?ε tbl)
  (set-case-syntax-pair ?Ζ ?ζ tbl)
  (set-case-syntax-pair ?Η ?η tbl)
  (set-case-syntax-pair ?Θ ?θ tbl)
  (set-case-syntax-pair ?Ι ?ι tbl)
  (set-case-syntax-pair ?Κ ?κ tbl)
  (set-case-syntax-pair ?Λ ?λ tbl)
  (set-case-syntax-pair ?Μ ?μ tbl)
  (set-case-syntax-pair ?Ν ?ν tbl)
  (set-case-syntax-pair ?Ξ ?ξ tbl)
  (set-case-syntax-pair ?Ο ?ο tbl)
  (set-case-syntax-pair ?Π ?π tbl)
  (set-case-syntax-pair ?Ρ ?ρ tbl)
  (set-case-syntax-pair ?Σ ?σ tbl)
  (set-case-syntax-pair ?Τ ?τ tbl)
  (set-case-syntax-pair ?Υ ?υ tbl)
  (set-case-syntax-pair ?Φ ?φ tbl)
  (set-case-syntax-pair ?Χ ?χ tbl)
  (set-case-syntax-pair ?Ψ ?ψ tbl)
  (set-case-syntax-pair ?Ω ?ω tbl)
  (set-case-syntax-pair ?Ϊ ?ϊ tbl)
  (set-case-syntax-pair ?Ϋ ?ϋ tbl)
  (set-case-syntax-pair ?Ώ ?ώ tbl)
  (set-case-syntax-pair ?Ύ ?ύ tbl)
  (set-case-syntax-pair ?Ό ?ό tbl)
  (set-case-syntax-pair ?Ά ?ά tbl)
  (set-case-syntax-pair ?Έ ?έ tbl)
  (set-case-syntax-pair ?Ή ?ή tbl)
  (set-case-syntax-pair ?Ί ?ί tbl))

;; Hebrew character set (ISO-8859-8)

(modify-category-entry '(#x590 . #x5f4) ?w)

;; (modify-syntax-entry (make-char 'hebrew-iso8859-8 208) ".") ; PASEQ
;; (modify-syntax-entry (make-char 'hebrew-iso8859-8 211) ".") ; SOF PASUQ
(modify-syntax-entry #x5be ".") ; MAQAF
(modify-syntax-entry #x5c0 ".") ; PASEQ
(modify-syntax-entry #x5c3 ".") ; SOF PASUQ
(modify-syntax-entry #x5f3 ".") ; GERESH
(modify-syntax-entry #x5f4 ".") ; GERSHAYIM

;; (let ((c 224))
;;   (while (< c 251)
;;     (modify-syntax-entry (make-char 'hebrew-iso8859-8 c) "w")
;;     (setq c (1+ c))))
;; (modify-syntax-entry (make-char 'hebrew-iso8859-8 160) "w") ; NBSP

;; Indian character set (IS 13194 and other Emacs original Indian charsets)

;; (modify-category-entry (make-char 'indian-is13194) ?i)
;; (modify-category-entry (make-char 'indian-2-column) ?I)
;; (modify-category-entry (make-char 'indian-glyph) ?I)
;; Unicode Devanagari block
(modify-category-entry '(#x901 . #x970) ?i)
(map-charset-chars #'modify-category-entry 'indian-is13194 ?i)
(map-charset-chars #'modify-category-entry 'indian-2-column ?i)

;;; Commented out since the categories appear not to be used anywhere
;;; and word syntax is the default.
;; (let ((deflist				;
;; 	'(;; chars	syntax	category
;; 	  ("ँंः"	"w"	?7) ; vowel-modifying diacritical mark
;; 				    ; chandrabindu, anuswar, visarga
;; 	  ("अ-ऍ"	"w"	?1) ; base (independent) vowel
;; 	  ("क-ह"	"w"	?0) ; consonant
;; 	  ("ा-ॉ"	"w"	?8) ; matra
;; 	  ("०-९"	"w"	?6) ; digit
;; 	  ))
;;       elm chars len syntax category to ch i)
;;   (while deflist
;;     (setq elm (car deflist))
;;     (setq chars (car elm)
;; 	  len (length chars)
;; 	  syntax (nth 1 elm)
;; 	  category (nth 2 elm)
;; 	  i 0)
;;     (while (< i len)
;;       (if (= (aref chars i) ?-)
;; 	  (setq i (1+ i)
;; 		to (aref chars i))
;; 	(setq ch (aref chars i)
;; 	      to ch))
;;       (while (<= ch to)
;; 	(modify-syntax-entry ch syntax)
;; 	(modify-category-entry ch category)
;; 	(setq ch (1+ ch)))
;;       (setq i (1+ i)))
;;     (setq deflist (cdr deflist))))


;; Japanese character set (JISX0201-kana, JISX0201-roman, JISX0208, JISX0212)

(map-charset-chars
 #'(lambda (char ignore)
     (if (consp char)
	 (let ((from (car char))
	       (to (car char)))
	   (while (<= from to)
	     (modify-category-entry from ?k)
	     (setq from (1+ from))))
       (modify-category-entry char ?k)))
 'katakana-jisx0201)

(map-charset-chars
 #'(lambda (char ignore)
     (if (consp char)
	 (let ((from (car char))
	       (to (cdr char)))
	   (while (<= from to)
	     (modify-category-entry from ?r)
	     (setq from (1+ from))))
       (modify-category-entry char ?r)))
 'latin-jisx0201)
(dolist (l '(katakana-jisx0201 japanese-jisx0208 japanese-jisx0212))
  (map-charset-chars
   #'(lambda (char ignore)
       (if (consp char)
	   (let ((from (car char))
		 (to (cdr char)))
	     (while (<= from to)
	       (modify-category-entry from ?j)
	       (modify-category-entry from ?\|)
	       (setq from (1+ from))))
	 (modify-category-entry char ?j)
	 (modify-category-entry char ?\|)))
   l))

;; Unicode equivalents of JISX0201-kana
(let ((c #xff61))
  (while (<= c #xff9f)
    (modify-category-entry c ?k)
    (modify-category-entry c ?j)
    (modify-category-entry c ?\|)
    (setq c (1+ c))))

;; Katakana block
(let ((c #x30a0))
  (while (<= c #x30ff)
    ;; ?K is double width, ?k isn't specified
    (modify-category-entry c ?K)
    ;;(modify-category-entry (decode-char 'ucs c) ?j)
    (modify-category-entry c ?\|) 
    (setq c (1+ c))))

;; Hiragana block
(let ((c #x3040))
  (while (<= c #x309f)
    ;; ?H is actually defined to be double width
    (modify-category-entry c ?H)
    ;;(modify-category-entry (decode-char 'ucs c) ?j)
    (modify-category-entry c ?\|) 
    (setq c (1+ c))))

;; JISX0208
(modify-syntax-entry (cons (decode-char 'japanese-jisx0208 #x2121)
			   (decode-char 'japanese-jisx0208 #x227E)) "_")
(modify-syntax-entry (cons (decode-char 'japanese-jisx0208 #x2821)
			   (decode-char 'japanese-jisx0208 #x287E)) "_")
(let ((chars '(?���� ?���� ?���� ?���� ?���� ?���� ?���� ?���� ?���� ?���� ?���� ?����)))
  (dolist (elt chars)
    (modify-syntax-entry (car chars) "w")))
(modify-syntax-entry ?\���� "(����")
(modify-syntax-entry ?\���� "(����")
(modify-syntax-entry ?\���� "(����")
(modify-syntax-entry ?\���� "(����")
(modify-syntax-entry ?\���� "(����")
(modify-syntax-entry ?\���� ")����")
(modify-syntax-entry ?\���� ")����")
(modify-syntax-entry ?\���� ")����")
(modify-syntax-entry ?\���� ")����")
(modify-syntax-entry ?\���� ")����")

(modify-category-entry (cons (decode-char 'japanese-jisx0208 #x2321)
			     (decode-char 'japanese-jisx0208 #x237E)) ?A)
(modify-category-entry (cons (decode-char 'japanese-jisx0208 #x2421)
			     (decode-char 'japanese-jisx0208 #x247E)) ?H)
(modify-category-entry (cons (decode-char 'japanese-jisx0208 #x2521)
			     (decode-char 'japanese-jisx0208 #x257E)) ?K)
(modify-category-entry (cons (decode-char 'japanese-jisx0208 #x2621)
			     (decode-char 'japanese-jisx0208 #x267E)) ?G)
(modify-category-entry (cons (decode-char 'japanese-jisx0208 #x2721)
			     (decode-char 'japanese-jisx0208 #x277E)) ?Y)
(modify-category-entry (cons (decode-char 'japanese-jisx0208 #x3021)
			     (decode-char 'japanese-jisx0208 #x7E7E)) ?C)
(modify-category-entry ?���� ?K)
(let ((chars '(?���� ?����)))
  (while chars
    (modify-category-entry (car chars) ?K)
    (modify-category-entry (car chars) ?H)
    (setq chars (cdr chars))))
(let ((chars '(?���� ?���� ?���� ?���� ?���� ?���� ?���� ?���� ?����)))
  (while chars
    (modify-category-entry (car chars) ?C)
    (setq chars (cdr chars))))

;; JISX0212
;; (modify-syntax-entry (make-char 'japanese-jisx0212) "w")
;; (modify-syntax-entry (make-char 'japanese-jisx0212 33) "_")
;; (modify-syntax-entry (make-char 'japanese-jisx0212 34) "_")
;; (modify-syntax-entry (make-char 'japanese-jisx0212 35) "_")
 
(modify-syntax-entry (cons (decode-char 'japanese-jisx0212 #x2121)
			   (decode-char 'japanese-jisx0212 #x237E))
		     "_")

;; JISX0201-Kana
;; (modify-syntax-entry (make-char 'katakana-jisx0201) "w")
(let ((chars '(?｡ ?､ ?･)))
  (while chars
    (modify-syntax-entry (car chars) ".")
    (setq chars (cdr chars))))

(modify-syntax-entry ?\｢ "(｣")
(modify-syntax-entry ?\｣ "(｢")

;; Korean character set (KSC5601)

;; Fixme: re-instate these

;; (modify-syntax-entry (make-char 'korean-ksc5601) "w")
;; (modify-syntax-entry (make-char 'korean-ksc5601 33) "_")
;; (modify-syntax-entry (make-char 'korean-ksc5601 34) "_")
;; (modify-syntax-entry (make-char 'korean-ksc5601 38) "_")
;; (modify-syntax-entry (make-char 'korean-ksc5601 39) "_")
;; (modify-syntax-entry (make-char 'korean-ksc5601 40) "_")
;; (modify-syntax-entry (make-char 'korean-ksc5601 41) "_")

;; (modify-category-entry (make-char 'korean-ksc5601) ?h)
;; (modify-category-entry (make-char 'korean-ksc5601 35) ?A)
;; (modify-category-entry (make-char 'korean-ksc5601 37) ?G)
;; (modify-category-entry (make-char 'korean-ksc5601 42) ?H)
;; (modify-category-entry (make-char 'korean-ksc5601 43) ?K)
;; (modify-category-entry (make-char 'korean-ksc5601 44) ?Y)

;; Latin

(modify-category-entry '(#x80 . #x024F) ?l)

;; Lao character set

(modify-category-entry '(#xe80 . #xeff) ?o)
(map-charset-chars #'modify-category-entry 'lao ?o)

;; Fixme: check this.  Lao characters in HELLO seem to have all the categories
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

;; Unicode (mule-unicode-0100-24ff)

(let ((tbl (standard-case-table)) c)

;; In some languages, U+0049 LATIN CAPITAL LETTER I and U+0131 LATIN
;; SMALL LETTER DOTLESS I make a case pair, and so do U+0130 LATIN
;; CAPITAL LETTER I WITH DOT ABOVE and U+0069 LATIN SMALL LETTER I.
;; Thus we have to check language-environment to handle casing
;; correctly.  Currently only I<->i is available.

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
;;;  (set-case-syntax-pair ?Ÿ ?ÿ tbl)	; these two have different length!
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
  (setq c #x2010)
  (while (<= c #x2027)
    (set-case-syntax c "_" tbl)
    (setq c (1+ c)))

  ;; Roman numerals
  (setq c #x2160)
  (while (<= c #x216f)
    (set-case-syntax-pair c (+ c #x10) tbl)
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

  ;; Ohm, Kelvin, Angstrom
;;;  (set-case-syntax-pair ?Ω ?ω tbl)
;;;  These mess up the case conversion of k and å.
;;;  (set-case-syntax-pair ?K ?k tbl)
;;;  (set-case-syntax-pair ?Å ?å tbl)

  ;; Combining diacritics
  (modify-category-entry '(#x300 . #x362) ?^)
  ;; Combining marks
  (modify-category-entry '(#x20d0 . #x20e3) ?^)

  ;; Fixme: syntax for symbols &c
  )

;;; Setting word boundary.

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
 

(set-char-table-range printable-chars '(0 . 31) nil)
(set-char-table-range printable-chars  '(127 . 159) nil)

;; CJK double width characters.
(let ((l '((#x1100 . #x11FF)
	   (#x2E80 . #x9FAF)
	   (#xAC00 . #xD7AF)
	   (#xF900 . #xFAFF)
	   (#xFE30 . #xFE4F)
	   (#xFF00 . #xFF5F)
	   (#xFFE0 . #xFFEF))))
  (dolist (elt l)
    (set-char-table-range char-width-table
			  (cons (car elt) (cdr elt))
			  2)))
(map-charset-chars
 #'(lambda (range ignore) (set-char-table-range char-width-table range 2))
 'japanese-jisx0208)
(map-charset-chars
 #'(lambda (range ignore) (set-char-table-range char-width-table range 2))
 'japanese-jisx0212)
(map-charset-chars
 #'(lambda (range ignore) (set-char-table-range char-width-table range 2))
 'japanese-jisx0213-1)
(map-charset-chars
 #'(lambda (range ignore) (set-char-table-range char-width-table range 2))
 'japanese-jisx0213-2)

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

;;; Local Variables:
;;; coding: utf-8-emacs
;;; End:

;;; characters.el ends here
